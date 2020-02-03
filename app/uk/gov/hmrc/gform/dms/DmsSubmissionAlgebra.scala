/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.dms

import java.time.{ Clock, LocalDateTime }
import java.util.Base64

import akka.util.ByteString
import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.apache.pdfbox.pdmodel.PDDocument
import uk.gov.hmrc.gform.fileupload.{ Attachments, FileUploadAlgebra, FileUploadFrontendConnector }
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorAlgebra
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DmsSubmission
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Constant, FormTemplateId, TextExpression }
import uk.gov.hmrc.gform.submission.{ DmsMetaData, PdfAndXmlSummaries, PdfSummary, Submission }
import uk.gov.hmrc.http.HeaderCarrier
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.functor._
import cats.syntax.flatMap._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait DmsSubmissionAlgebra[F[_]] {
  def submitToDms(dmsHtmlSubmission: DmsHtmlSubmission)(implicit hc: HeaderCarrier): F[EnvelopeId]
  def submitPdfToDms(pdfBytes: Array[Byte], dmsMetaData: DmsMetadata)(implicit hc: HeaderCarrier): F[EnvelopeId]
  def submitPdfToDmsWithAttachments(html: String, attachments: List[FileAttachment], metaData: DmsMetadata)(
    implicit hc: HeaderCarrier): F[EnvelopeId]
}

class DmsSubmissionService[F[_]](
  fileUpload: FileUploadAlgebra[F],
  pdfGenerator: PdfGeneratorAlgebra[F],
  documentLoader: Array[Byte] => PDDocument,
  formExpiryDays: Long)(implicit clock: Clock, M: Monad[F])
    extends DmsSubmissionAlgebra[F] {

  override def submitToDms(dmsHtmlSubmission: DmsHtmlSubmission)(implicit hc: HeaderCarrier): F[EnvelopeId] =
    pdfGenerator
      .generatePDFBytes(decode(dmsHtmlSubmission.b64html))
      .flatMap { byteArray =>
        submitPdfToDms(byteArray, dmsHtmlSubmission.metadata)
      }

  override def submitPdfToDms(pdfBytes: Array[Byte], metadata: DmsMetadata)(
    implicit hc: HeaderCarrier): F[EnvelopeId] = {
    val formTemplateId = FormTemplateId(metadata.dmsFormId)
    for {
      envId <- fileUpload.createEnvelope(formTemplateId, LocalDateTime.now(clock).plusDays(formExpiryDays))
      pdfDoc = documentLoader(pdfBytes)
      pdfSummary = PdfSummary(pdfDoc.getNumberOfPages.toLong, pdfBytes)
      _ = pdfDoc.close()
      submission = DmsSubmissionService.createSubmission(metadata, envId, LocalDateTime.now(clock), 0)
      summaries = PdfAndXmlSummaries(pdfSummary)
      dmsSubmission = DmsSubmissionService.createDmsSubmission(metadata)
      _ <- fileUpload.submitEnvelope(submission, summaries, dmsSubmission)
    } yield envId
  }

  override def submitPdfToDmsWithAttachments(html: String, attachments: List[FileAttachment], metaData: DmsMetadata)(
    implicit hc: HeaderCarrier): F[EnvelopeId] = {

    val formTemplateId = FormTemplateId(metaData.dmsFormId)

    for {
      pdfBytes   <- pdfGenerator.generatePDFBytes(decode(html))
      envelopeId <- fileUpload.createEnvelope(formTemplateId, LocalDateTime.now(clock).plusDays(formExpiryDays))
      pdfDoc = documentLoader(pdfBytes)
      pdfSummary = PdfSummary(pdfDoc.getNumberOfPages.toLong, pdfBytes)
      _ = pdfDoc.close()
      _ <- attachments.traverse { fileAttachment =>
            fileUpload.uploadAttachment(envelopeId, fileAttachment)
          }
      submission = DmsSubmissionService
        .createSubmission(metaData, envelopeId, LocalDateTime.now(clock), attachments.size)
      summaries = PdfAndXmlSummaries(pdfSummary)
      dmsSubmission = DmsSubmissionService.createDmsSubmission(metaData)
      _ <- fileUpload.submitEnvelope(submission, summaries, dmsSubmission)
    } yield (envelopeId)

  }

  private val decode = (s: String) => new String(Base64.getDecoder.decode(s))

}

object DmsSubmissionService {
  def createDmsSubmission(metadata: DmsMetadata): DmsSubmission =
    DmsSubmission(
      metadata.dmsFormId,
      TextExpression(Constant(metadata.customerId)),
      metadata.classificationType,
      metadata.businessArea)

  def createSubmission(
    metadata: DmsMetadata,
    envId: EnvelopeId,
    submittedDate: LocalDateTime,
    attachments: Int): Submission =
    Submission(
      FormId(metadata.dmsFormId),
      submittedDate,
      SubmissionRef(envId),
      envId,
      attachments,
      DmsMetaData(FormTemplateId(metadata.dmsFormId), metadata.customerId)
    )
}
