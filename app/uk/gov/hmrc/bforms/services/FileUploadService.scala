/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.services

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.bforms.exceptions.InvalidState
import uk.gov.hmrc.bforms.model.{ EnvelopeId, FileId, MetadataXml }
import uk.gov.hmrc.bforms.typeclasses.{ HttpExecutor, UploadFile }
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.bforms.connectors.{ FusConnector, FusFeConnector }
import uk.gov.hmrc.bforms.model._
import uk.gov.hmrc.bforms.typeclasses.{ FusUrl, FusFeUrl, ServiceUrl, CreateEnvelope }

class FileUploadService(fusConnector: FusConnector, fusFeConnector: FusFeConnector) {

  val dmsMetaData = DmsMetaData(
    formId = "some-fomr-id",
    formNino = None,
    authNino = None,
    classificationType = "some-classification-type",
    businessArea = "some-business-area"
  )

  val submission = Submission(
    submittedDate = LocalDateTime.now(),
    submissionRef = "some-submission-ref",
    dmsMetaData = dmsMetaData,
    submissionMark = Some("submission-mark"),
    casKey = Some("some-cas-key")
  )

  val pdfSummary = PdfSummary(
    submissionRef = "some-submission-ref",
    numberOfPages = 10L,
    pdfContent = Array.empty[Byte]
  )

  val submissionAndPdf = SubmissionAndPdf(
    submission = submission,
    pdfSummary = pdfSummary
  )

  def createEnvelop(
    implicit
    hc: HeaderCarrier,
    ec: ExecutionContext,
    fusUrl: ServiceUrl[FusUrl],
    fusFeUrl: ServiceUrl[FusFeUrl]
  ) = {
    for {
      envelopeId <- HttpExecutor(fusUrl, CreateEnvelope, fusConnector.envelopeRequest("formTypeRef")).map(fusConnector.extractEnvelopId)
      res <- { // TODO use cats EitherT to get rid of this pattern matching
        envelopeId match {
          case Right(envelopeId) =>
            val uf = UploadFile(envelopeId, FileId("xmlDocument"))
            HttpExecutor(fusFeUrl, uf, MetadataXml.getXml("submission-ref", "reconciliation-id", submissionAndPdf).toString().getBytes)
          case l @ Left(_) => Future.successful(l)
        }
      }

    } yield {
      s"Envelope id: $envelopeId"
    }
    /* fusConnector.createEnvelope("testFormTypeId").map { envelopeId =>
     *
     * } */
  }
}
