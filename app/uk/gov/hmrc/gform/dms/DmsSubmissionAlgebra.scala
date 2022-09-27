/*
 * Copyright 2022 HM Revenue & Customs
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
import cats.Monad
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import org.apache.pdfbox.pdmodel.PDDocument
import uk.gov.hmrc.gform.config.FileInfoConfig
import uk.gov.hmrc.gform.fileupload.FileUploadAlgebra
import uk.gov.hmrc.gform.formtemplate.FormTemplateAlgebra
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorAlgebra
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DataOutputFormat
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Constant, FormTemplateId }
import uk.gov.hmrc.gform.submission.{ DmsMetaData, PdfAndXmlSummaries, PdfSummary, Submission, SubmissionId }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext

trait DmsSubmissionAlgebra[F[_]] {
  def submitToDms(dmsHtmlSubmission: DmsHtmlSubmission, fileAttachments: List[FileAttachment])(implicit
    hc: HeaderCarrier
  ): F[EnvelopeId]
  def submitPdfToDms(pdfBytes: Array[Byte], dmsMetaData: DmsMetadata, fileAttachments: List[FileAttachment])(implicit
    hc: HeaderCarrier
  ): F[EnvelopeId]
}

class DmsSubmissionService[F[_]](
  fileUpload: FileUploadAlgebra[F],
  pdfGenerator: PdfGeneratorAlgebra[F],
  formTemplateAlgebra: FormTemplateAlgebra[F],
  documentLoader: Array[Byte] => PDDocument,
  formExpiryDays: Long
)(implicit clock: Clock, M: Monad[F], ec: ExecutionContext)
    extends DmsSubmissionAlgebra[F] {

  override def submitToDms(dmsHtmlSubmission: DmsHtmlSubmission, fileAttachments: List[FileAttachment])(implicit
    hc: HeaderCarrier
  ): F[EnvelopeId] =
    pdfGenerator
      .generatePDFBytesLocal(decode(dmsHtmlSubmission.html))
      .flatMap { byteArray =>
        submitPdfToDms(byteArray, dmsHtmlSubmission.metadata, fileAttachments)
      }

  override def submitPdfToDms(pdfBytes: Array[Byte], metadata: DmsMetadata, fileAttachments: List[FileAttachment])(
    implicit hc: HeaderCarrier
  ): F[EnvelopeId] = {
    val formTemplateId: FormTemplateId = FormTemplateId(metadata.dmsFormId)
    for {
      formTemplate <- formTemplateAlgebra.get(formTemplateId)
      envId <-
        fileUpload.createEnvelope(
          formTemplateId,
          FileInfoConfig.allAllowedFileTypes,
          LocalDateTime.now(clock).plusDays(formExpiryDays),
          None,
          formTemplate.objectStore.getOrElse(false)
        )
      pdfDoc = documentLoader(pdfBytes)
      pdfSummary = PdfSummary(pdfDoc.getNumberOfPages.toLong, pdfBytes)
      _ = pdfDoc.close()
      _ <- fileAttachments.traverse { fileAttachment =>
             fileUpload.uploadAttachment(envId, fileAttachment)
           }
      submission = DmsSubmissionService
                     .createSubmission(metadata, envId, LocalDateTime.now(clock), fileAttachments.size)
      summaries = PdfAndXmlSummaries(pdfSummary)
      hmrcDms = DmsSubmissionService.createHmrcDms(metadata)
      _ <- fileUpload.submitEnvelope(submission, summaries, hmrcDms)
    } yield envId
  }

  private val decode = (s: String) => new String(Base64.getDecoder.decode(s))

}

object DmsSubmissionService {

  def createHmrcDms(metadata: DmsMetadata): HmrcDms =
    HmrcDms(
      DestinationId("HmrcDms"),
      metadata.dmsFormId,
      Constant(metadata.customerId),
      metadata.classificationType,
      metadata.businessArea,
      "",
      true,
      Some(DataOutputFormat.XML),
      true,
      metadata.backscan,
      false
    )

  def createSubmission(
    metadata: DmsMetadata,
    envId: EnvelopeId,
    submittedDate: LocalDateTime,
    attachments: Int
  ): Submission =
    Submission(
      SubmissionId(FormId(metadata.dmsFormId), envId),
      submittedDate,
      SubmissionRef(envId),
      envId,
      attachments,
      DmsMetaData(FormTemplateId(metadata.dmsFormId), metadata.customerId)
    )
}
