/*
 * Copyright 2019 HM Revenue & Customs
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

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import akka.util.ByteString
import play.api.Logger
import uk.gov.hmrc.gform.fileupload.FileUploadService.FileIds._
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DmsSubmission
import uk.gov.hmrc.gform.submission.{ PdfAndXmlSummaries, Submission, SubmissionRef }
import uk.gov.hmrc.gform.time.TimeProvider

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HeaderCarrier

class FileUploadService(
  fileUploadConnector: FileUploadConnector,
  fileUploadFrontendConnector: FileUploadFrontendConnector,
  timeModule: TimeProvider = new TimeProvider)(implicit ex: ExecutionContext) {

  def createEnvelope(formTypeId: FormTemplateId, expiryDate: LocalDateTime)(
    implicit hc: HeaderCarrier): Future[EnvelopeId] = {
    val f = fileUploadConnector.createEnvelope(formTypeId, expiryDate)
    f map { id =>
      Logger.debug(s"env-id creation: $id")
    }
    f
  }

  def submitEnvelope(
    submission: Submission,
    summaries: PdfAndXmlSummaries,
    dmsSubmission: DmsSubmission,
    numberOfAttachments: Int)(implicit hc: HeaderCarrier): Future[Unit] = {

    val submissionRef: SubmissionRef = submission.submissionRef
    val envelopeId: EnvelopeId = submission.envelopeId
    Logger.debug(s"env-id submit: $envelopeId")
    val date = timeModule.localDateTime().format(DateTimeFormatter.ofPattern("yyyyMMdd"))
    val fileNamePrefix = s"$submissionRef-$date"
    val reconciliationId = ReconciliationId.create(submissionRef)
    val metadataXml = MetadataXml.xmlDec + "\n" + MetadataXml
      .getXml(submission, reconciliationId, summaries.pdfSummary, dmsSubmission, numberOfAttachments)

    def uploadPfdF: Future[Unit] =
      fileUploadFrontendConnector.upload(
        envelopeId,
        pdf,
        s"$fileNamePrefix-iform.pdf",
        ByteString(summaries.pdfSummary.pdfContent),
        ContentType.`application/pdf`)

    def uploadXmlF: Future[Unit] = uploadXml(xml, s"$fileNamePrefix-metadata.xml", metadataXml)

    def uploadAnyDataXmlF: Future[Unit] = summaries.xmlSummary match {
      case Some(elem) => uploadXml(data, s"$fileNamePrefix-data.xml", elem)
      case _          => Future.successful(())
    }

    def uploadRoboticsXmlF: Future[Unit] = summaries.roboticsXml match {
      case Some(elem) => uploadXml(roboticsXml, s"${submissionRef.value}-$date-robotic.xml", elem)
      case _          => Future.successful(())
    }

    def uploadXml(fileId: FileId, fileName: String, xml: String): Future[Unit] =
      fileUploadFrontendConnector
        .upload(envelopeId, fileId, fileName, ByteString(xml.getBytes), ContentType.`application/xml`)

    for {
      _ <- uploadPfdF
      _ <- uploadXmlF
      _ <- uploadAnyDataXmlF
      _ <- uploadRoboticsXmlF
      _ <- fileUploadConnector.routeEnvelope(RouteEnvelopeRequest(envelopeId, "dfs", "DMS"))
    } yield ()
  }

  def deleteFile(envelopeId: EnvelopeId, fileId: FileId)(implicit hc: HeaderCarrier): Future[Unit] =
    fileUploadConnector.deleteFile(envelopeId, fileId)
}

object FileUploadService {

  //forbidden keys. make sure they aren't used in templates
  object FileIds {
    val pdf = FileId("pdf")
    val xml = FileId("xmlDocument")
    val data = FileId("xmlSubmission")
    val roboticsXml = FileId("roboticsXml")
  }
}
