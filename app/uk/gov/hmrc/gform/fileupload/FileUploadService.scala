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

package uk.gov.hmrc.gform.fileupload

import org.apache.pekko.util.ByteString
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.core.FutureSyntax
import uk.gov.hmrc.gform.dms.FileAttachment
import uk.gov.hmrc.gform.objectstore.{ Envelope, MetadataXml, ObjectStoreAlgebra, ReconciliationId, RouteEnvelopeRequest }
import uk.gov.hmrc.gform.objectstore.ObjectStoreService.FileIds._
import uk.gov.hmrc.gform.sdes.dms.DmsWorkItemAlgebra
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AllowedFileTypes, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination
import uk.gov.hmrc.gform.submission.{ PdfAndXmlSummaries, Submission }
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID
import scala.concurrent.{ ExecutionContext, Future }

class FileUploadService(
  fileUploadConnector: FileUploadConnector,
  fileUploadFrontendConnector: FileUploadFrontendConnector,
  timeModule: TimeProvider = new TimeProvider,
  objectStoreService: ObjectStoreAlgebra[Future],
  dmsWorkItemAlgebra: DmsWorkItemAlgebra[Future]
)(implicit ex: ExecutionContext)
    extends FileUploadAlgebra[Future] with FileDownloadAlgebra[Future] {
  private val logger = LoggerFactory.getLogger(getClass)

  def createEnvelope(
    formTemplateId: FormTemplateId,
    allowedFileTypes: AllowedFileTypes,
    expiryDate: LocalDateTime,
    fileSizeLimit: Option[Int],
    objectStore: Boolean
  )(implicit
    hc: HeaderCarrier
  ): Future[EnvelopeId] = {
    val f = fileUploadConnector.createEnvelope(formTemplateId, allowedFileTypes, expiryDate, fileSizeLimit, objectStore)
    f map { id =>
      logger.debug(s"env-id creation: $id")
    }
    f
  }

  def submitEnvelope(
    submission: Submission,
    summaries: PdfAndXmlSummaries,
    hmrcDms: HmrcDms,
    objectStore: Boolean,
    formTemplateId: FormTemplateId
  )(implicit
    hc: HeaderCarrier
  ): Future[Unit] = {
    logger.debug(s"env-id submit: ${submission.envelopeId}")
    val date = timeModule.localDateTime().format(DateTimeFormatter.ofPattern("yyyyMMdd"))
    val fileNamePrefix = s"${submission.submissionRef.withoutHyphens}-$date"

    def uploadPfdF: Future[Unit] = {
      val (fileId, fileNameSuffix) = {
        if (hmrcDms.instructionPdfFields.isDefined)
          (customerSummaryPdf, "customerSummary")
        else (pdf, "iform")
      }

      uploadFile(
        submission.envelopeId,
        fileId,
        s"$fileNamePrefix-$fileNameSuffix.pdf",
        ByteString(summaries.pdfSummary.pdfContent),
        ContentType.`application/pdf`,
        objectStore
      )
    }

    def uploadInstructionPdfF: Future[Unit] =
      summaries.instructionPdfSummary.fold(Future.successful(())) { iPdf =>
        uploadFile(
          submission.envelopeId,
          pdf,
          s"$fileNamePrefix-iform.pdf",
          ByteString(iPdf.pdfContent),
          ContentType.`application/pdf`,
          objectStore
        )
      }

    def uploadFormDataF: Future[Unit] =
      summaries.formDataXml
        .map(elem =>
          uploadFile(
            submission.envelopeId,
            formdataXml,
            s"$fileNamePrefix-formdata.xml",
            ByteString(elem.getBytes),
            ContentType.`application/xml`,
            objectStore
          )
        )
        .getOrElse(Future.successful(()))

    def uploadMetadataXmlF: Future[Unit] = {
      val reconciliationId = ReconciliationId.create(submission.submissionRef)
      val metadataXml = MetadataXml.xmlDec + "\n" + MetadataXml
        .getXml(
          submission,
          reconciliationId,
          summaries.instructionPdfSummary.fold(summaries.pdfSummary.numberOfPages)(_.numberOfPages),
          submission.noOfAttachments + summaries.instructionPdfSummary.fold(0)(_ => 1),
          hmrcDms
        )
      uploadFile(
        submission.envelopeId,
        xml,
        s"$fileNamePrefix-metadata.xml",
        ByteString(metadataXml.getBytes),
        ContentType.`application/xml`,
        objectStore
      )
    }

    def uploadRoboticsContentF: Future[Unit] = summaries.roboticsFile match {
      case Some(elem) =>
        val roboticsFileExtension = summaries.roboticsFileExtension.map(_.toLowerCase).getOrElse("xml")
        uploadFile(
          submission.envelopeId,
          roboticsFileId(roboticsFileExtension),
          s"$fileNamePrefix-robotic." + roboticsFileExtension,
          ByteString(elem.getBytes),
          getContentType(roboticsFileExtension),
          objectStore
        )
      case _ => Future.successful(())
    }

    for {
      _ <- uploadPfdF
      _ <- uploadInstructionPdfF
      _ <- uploadFormDataF
      _ <- uploadRoboticsContentF
      _ <- uploadMetadataXmlF
      _ <- if (objectStore)
             zipAndPushWorkItem(submission.envelopeId, formTemplateId, submission.submissionRef)
           else fileUploadConnector.routeEnvelope(RouteEnvelopeRequest(submission.envelopeId, "dfs", "DMS"))
    } yield ()
  }

  private def zipAndPushWorkItem(envelopeId: EnvelopeId, formTemplateId: FormTemplateId, submissionRef: SubmissionRef)(
    implicit hc: HeaderCarrier
  ): Future[Unit] =
    for {
      objectSummary <-
        objectStoreService.zipFiles(envelopeId, SdesDestination.Dms.objectStorePaths(envelopeId))
      _ <- dmsWorkItemAlgebra.pushWorkItem(envelopeId, formTemplateId, submissionRef, objectSummary)
    } yield ()

  private def getContentType(contentType: String) = contentType match {
    case "json" => ContentType.`application/json`
    case "pdf"  => ContentType.`application/pdf`
    case _      => ContentType.`application/xml`
  }

  private def uploadFile(
    envelopeId: EnvelopeId,
    fileId: FileId,
    fileName: String,
    content: ByteString,
    contentType: ContentType,
    objectStore: Boolean
  )(implicit
    hc: HeaderCarrier
  ): Future[Unit] =
    if (objectStore) {
      objectStoreService.uploadFile(envelopeId, fileId, fileName, content, contentType).void
    } else {
      fileUploadFrontendConnector
        .upload(envelopeId, fileId, fileName, content, contentType)
    }

  def getEnvelope(envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): Future[Envelope] =
    fileUploadConnector.getEnvelope(envelopeId)

  def getFileBytes(envelopeId: EnvelopeId, fileId: FileId)(implicit hc: HeaderCarrier): Future[ByteString] =
    fileUploadConnector.getFileBytes(envelopeId, fileId)

  def deleteFile(envelopeId: EnvelopeId, fileId: FileId)(implicit hc: HeaderCarrier): Future[Unit] =
    fileUploadConnector.deleteFile(envelopeId, fileId)

  def uploadAttachment(envelopeId: EnvelopeId, fileAttachment: FileAttachment, objectStore: Boolean)(implicit
    hc: HeaderCarrier
  ): Future[Unit] =
    uploadFile(
      envelopeId,
      FileId(UUID.randomUUID().toString),
      fileAttachment.filename.getFileName.toString,
      ByteString(fileAttachment.bytes),
      ContentType(fileAttachment.contentType.getOrElse("application/json")),
      objectStore
    )
}
