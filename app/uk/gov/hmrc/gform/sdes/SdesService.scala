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

import org.mongodb.scala.model.Filters.equal
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.sdes._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.objectstore.client.ObjectSummaryWithMd5

import java.util.Base64
import scala.concurrent.{ ExecutionContext, Future }

trait SdesAlgebra[F[_]] {
  def notifySDES(
    envelopeId: EnvelopeId,
    formTemplateId: FormTemplateId,
    submissionRef: SubmissionRef,
    objWithSummary: ObjectSummaryWithMd5
  )(implicit hc: HeaderCarrier): F[Unit]

  def saveSdesSubmission(sdesSubmission: SdesSubmission): F[Unit]

  def findSdesSubmission(correlationId: CorrelationId): F[Option[SdesSubmission]]

  def search(processed: Boolean, page: Int, pageSize: Int): F[SdesSubmissionPageData]
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
      _ <- sdesConnector.notifySDES(notifyRequest)
      _ <- saveSdesSubmission(sdesSubmission)
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

  override def search(processed: Boolean, page: Int, pageSize: Int): Future[SdesSubmissionPageData] = {

    val query = equal("isProcessed", processed)
    val sort = equal("submittedAt", -1)

    val skip = page * pageSize
    for {
      sdesSubmissions <- repoSdesSubmission.page(query, sort, skip, pageSize)
      count           <- repoSdesSubmission.count(query)
      countAll        <- repoSdesSubmission.countAll()
    } yield SdesSubmissionPageData(
      sdesSubmissions.map(s =>
        SdesSubmissionData(
          s.envelopeId.value,
          s.formTemplateId.value,
          s.submissionRef.value,
          s.submittedAt,
          s.status.toString,
          s.failureReason.getOrElse("")
        )
      ),
      count,
      countAll
    )
  }
}
