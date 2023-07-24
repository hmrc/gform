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

import cats.syntax.eq._
import cats.syntax.traverse._
import cats.syntax.functor._
import cats.syntax.show._
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Filters.{ equal, exists, lt }
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.envelope.EnvelopeAlgebra
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sdes.datastore.DataStoreWorkItemAlgebra
import uk.gov.hmrc.gform.sdes.dms.DmsWorkItemAlgebra
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.sdes.NotificationStatus.{ FileReady, fromName }
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination.{ DataStore, Dms }
import uk.gov.hmrc.gform.sharedmodel.sdes._
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.objectstore.client.ObjectSummaryWithMd5

import java.time.LocalDateTime
import scala.concurrent.{ ExecutionContext, Future }

trait SdesAlgebra[F[_]] {

  def notifySDES(
    correlationId: CorrelationId,
    envelopeId: EnvelopeId,
    formTemplateId: FormTemplateId,
    submissionRef: SubmissionRef,
    notifyRequest: SdesNotifyRequest,
    destination: SdesDestination
  )(implicit
    hc: HeaderCarrier
  ): F[HttpResponse]

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
    showBeforeAt: Option[Boolean],
    destination: Option[SdesDestination]
  ): F[SdesSubmissionPageData]

  def deleteSdesSubmission(correlation: CorrelationId): F[Unit]
}

class SdesService(
  dmsConnector: SdesConnector,
  dataStoreConnector: SdesConnector,
  repoSdesSubmission: Repo[SdesSubmission],
  dmsWorkItemAlgebra: DmsWorkItemAlgebra[Future],
  dataStoreWorkItemAlgebra: DataStoreWorkItemAlgebra[Future],
  envelopeAlgebra: EnvelopeAlgebra[Future],
  timeProvider: TimeProvider
)(implicit
  ec: ExecutionContext
) extends SdesAlgebra[Future] {
  private val logger = LoggerFactory.getLogger(getClass)

  override def notifySDES(
    correlationId: CorrelationId,
    envelopeId: EnvelopeId,
    formTemplateId: FormTemplateId,
    submissionRef: SubmissionRef,
    notifyRequest: SdesNotifyRequest,
    destination: SdesDestination
  )(implicit
    hc: HeaderCarrier
  ): Future[HttpResponse] = {
    val sdesSubmission =
      SdesSubmission.createSdesSubmission(
        correlationId,
        envelopeId,
        formTemplateId,
        submissionRef,
        destination,
        timeProvider.instantLocal()
      )
    for {
      res <- if (destination === DataStore) dataStoreConnector.notifySDES(notifyRequest)
             else dmsConnector.notifySDES(notifyRequest)
      _ <- saveSdesSubmission(sdesSubmission.copy(submittedAt = Some(timeProvider.instantLocal()), status = FileReady))
    } yield res
  }

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
    showBeforeAt: Option[Boolean],
    destination: Option[SdesDestination]
  ): Future[SdesSubmissionPageData] = {

    val queryByTemplateId =
      formTemplateId.fold(exists("_id"))(t => equal("formTemplateId", t.value))
    val queryByProcessed =
      processed.fold(queryByTemplateId)(p => Filters.and(equal("isProcessed", p), queryByTemplateId))
    val queryByStatus =
      status.fold(queryByProcessed)(s => Filters.and(equal("status", fromName(s)), queryByProcessed))
    val queryByDestination =
      destination.fold(queryByStatus)(d =>
        Filters.and(equal("destination", SdesDestination.fromName(d)), queryByStatus)
      )
    val query = if (showBeforeAt.getOrElse(false)) {
      Filters.and(queryByDestination, lt("createdAt", LocalDateTime.now().minusHours(10)))
    } else {
      queryByDestination
    }

    val queryNotProcessed = Filters.and(equal("isProcessed", false), queryByTemplateId)

    val sort = equal("createdAt", -1)

    val skip = page * pageSize
    for {
      sdesSubmissions <- repoSdesSubmission.page(query, sort, skip, pageSize)
      sdesSubmissionData <- sdesSubmissions.traverse(sdesSubmission =>
                              for {
                                numberOfFiles <-
                                  sdesSubmission.destination match {
                                    case Some(Dms) =>
                                      envelopeAlgebra
                                        .get(sdesSubmission.envelopeId)
                                        .map(_.files.count(_.fileId =!= FileUploadService.FileIds.dataStore.value))
                                    case _ => Future.successful(1)
                                  }
                              } yield SdesSubmissionData.fromSdesSubmission(sdesSubmission, numberOfFiles)
                            )
      count    <- repoSdesSubmission.count(queryNotProcessed)
      countAll <- repoSdesSubmission.count(queryByTemplateId)
    } yield SdesSubmissionPageData(sdesSubmissionData, count, countAll)

  }

  override def notifySDES(sdesSubmission: SdesSubmission, objWithSummary: ObjectSummaryWithMd5)(implicit
    hc: HeaderCarrier
  ): Future[HttpResponse] =
    for {
      res <- sdesSubmission.destination match {
               case Some(DataStore) =>
                 val notifyRequest =
                   dataStoreWorkItemAlgebra.createNotifyRequest(objWithSummary, sdesSubmission._id.value)
                 dataStoreConnector.notifySDES(notifyRequest)
               case _ =>
                 val notifyRequest = dmsWorkItemAlgebra.createNotifyRequest(objWithSummary, sdesSubmission._id.value)
                 dmsConnector.notifySDES(notifyRequest)
             }
      _ <-
        saveSdesSubmission(
          sdesSubmission.copy(
            submittedAt = Some(sdesSubmission.submittedAt.getOrElse(timeProvider.instantLocal())),
            lastUpdated = Some(timeProvider.instantLocal()),
            isProcessed = false,
            status = FileReady,
            failureReason = None,
            confirmedAt = None
          )
        )
    } yield res

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
