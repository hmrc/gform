/*
 * Copyright 2023 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.gform.sdes

import cats.syntax.functor._
import cats.syntax.show._
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Filters.{ equal, exists, lt }
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.sdes.NotificationStatus.{ FileReady, fromName }
import uk.gov.hmrc.gform.sharedmodel.sdes._
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.objectstore.client.ObjectSummaryWithMd5

import java.time.{ Instant, LocalDateTime }
import java.util.Base64
import scala.concurrent.{ ExecutionContext, Future }

trait SdesAlgebra[F[_]] {
  def notifySDES(
    envelopeId: EnvelopeId,
    formTemplateId: FormTemplateId,
    submissionRef: SubmissionRef,
    objWithSummary: ObjectSummaryWithMd5
  )(implicit hc: HeaderCarrier): F[Unit]

  def notifySDES(sdesSubmission: SdesSubmission, objWithSummary: ObjectSummaryWithMd5)(implicit
    hc: HeaderCarrier
  ): F[HttpResponse]

  def saveSdesSubmission(sdesSubmission: SdesSubmission): F[Unit]

  def findSdesSubmission(correlationId: CorrelationId): F[Option[SdesSubmission]]

  def search(
    page: Int,
    pageSize: Int,
    processed: Option[Boolean],
    formTemplateId: Option[FormTemplateId],
    status: Option[NotificationStatus],
    showBeforeDate: Option[Boolean]
  ): F[SdesSubmissionPageData]

  def deleteSdesSubmission(correlation: CorrelationId): F[Unit]
}

class SdesService(
  sdesConnector: SdesConnector,
  repoSdesSubmission: Repo[SdesSubmission],
  informationType: String,
  recipientOrSender: String,
  fileLocationUrl: String
)(implicit
  ec: ExecutionContext
) extends SdesAlgebra[Future] {
  private val logger = LoggerFactory.getLogger(getClass)

  override def notifySDES(
    envelopeId: EnvelopeId,
    formTemplateId: FormTemplateId,
    submissionRef: SubmissionRef,
    objWithSummary: ObjectSummaryWithMd5
  )(implicit
    hc: HeaderCarrier
  ): Future[Unit] = {
    val sdesSubmission = SdesSubmission.createSdesSubmission(envelopeId, formTemplateId, submissionRef)
    val notifyRequest = createNotifyRequest(objWithSummary, sdesSubmission._id.value)
    for {
      _ <- saveSdesSubmission(sdesSubmission)
      _ <- sdesConnector
             .notifySDES(notifyRequest)
             .map(_ => saveSdesSubmission(sdesSubmission.copy(submittedAt = Some(Instant.now), status = FileReady)))
             .recoverWith { case _ =>
               Future.unit //it doesn't break the submission when the service is unavailable
             }
    } yield ()
  }

  private def createNotifyRequest(
    objSummary: ObjectSummaryWithMd5,
    correlationId: String
  ): SdesNotifyRequest =
    SdesNotifyRequest(
      informationType,
      FileMetaData(
        recipientOrSender,
        objSummary.location.fileName,
        s"$fileLocationUrl${objSummary.location.asUri}",
        FileChecksum(value = Base64.getDecoder.decode(objSummary.contentMd5.value).map("%02x".format(_)).mkString),
        objSummary.contentLength,
        List()
      ),
      FileAudit(correlationId)
    )

  override def saveSdesSubmission(sdesSubmission: SdesSubmission): Future[Unit] =
    repoSdesSubmission.upsert(sdesSubmission).toFuture

  override def findSdesSubmission(correlationId: CorrelationId): Future[Option[SdesSubmission]] =
    repoSdesSubmission.find(correlationId.value)

  override def search(
    page: Int,
    pageSize: Int,
    processed: Option[Boolean],
    formTemplateId: Option[FormTemplateId],
    status: Option[NotificationStatus],
    showBeforeDate: Option[Boolean]
  ): Future[SdesSubmissionPageData] = {

    val queryByTemplateId =
      formTemplateId.fold(exists("_id"))(t => equal("formTemplateId", t.value))
    val queryByProcessed =
      processed.fold(queryByTemplateId)(p => Filters.and(equal("isProcessed", p), queryByTemplateId))
    val queryByStatus =
      status.fold(queryByProcessed)(s => Filters.and(equal("status", fromName(s)), queryByProcessed))
    val query = if (showBeforeDate.getOrElse(false)) {
      Filters.and(queryByStatus, lt("createdAt", LocalDateTime.now().minusHours(10)))
    } else {
      queryByStatus
    }

    val queryNotProcessed = Filters.and(equal("isProcessed", false), queryByTemplateId)

    val sort = equal("createdAt", -1)

    val skip = page * pageSize
    for {
      sdesSubmissions <- repoSdesSubmission.page(query, sort, skip, pageSize)
      count           <- repoSdesSubmission.count(queryNotProcessed)
      countAll        <- repoSdesSubmission.count(queryByTemplateId)
    } yield SdesSubmissionPageData(
      sdesSubmissions.map(s =>
        SdesSubmissionData(
          s._id,
          s.envelopeId,
          s.formTemplateId,
          s.submissionRef,
          s.submittedAt,
          s.status,
          s.failureReason.getOrElse(""),
          s.createdAt,
          s.lastUpdated
        )
      ),
      count,
      countAll
    )

  }

  override def notifySDES(sdesSubmission: SdesSubmission, objWithSummary: ObjectSummaryWithMd5)(implicit
    hc: HeaderCarrier
  ): Future[HttpResponse] = {
    val notifyRequest = createNotifyRequest(objWithSummary, sdesSubmission._id.value)
    for {
      res <- sdesConnector.notifySDES(notifyRequest)
      _ <-
        saveSdesSubmission(
          sdesSubmission.copy(
            submittedAt = Some(sdesSubmission.submittedAt.getOrElse(Instant.now)),
            lastUpdated = Some(Instant.now),
            isProcessed = false,
            status = FileReady,
            failureReason = None,
            confirmedAt = None
          )
        )
    } yield res
  }

  override def deleteSdesSubmission(correlationId: CorrelationId): Future[Unit] =
    repoSdesSubmission
      .delete(correlationId.value)
      .as(logger.info(show"SdesService.deleteSdesSubmission(${correlationId.value}) - deleting)"))
      .value
      .flatMap {
        case Left(UnexpectedState(error)) => Future.failed(new Exception(error))
        case Right(unit)                  => Future.successful(unit)
      }
}
