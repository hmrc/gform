/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.submission.destinations

import org.apache.pekko.util.ByteString
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.formtemplate.FormTemplateAlgebra
import uk.gov.hmrc.gform.objectstore.MetadataXml.xmlDec
import uk.gov.hmrc.gform.objectstore.{ ObjectStoreAlgebra, ObjectStorePaths }
import uk.gov.hmrc.gform.pdfgenerator.{ FopService, PdfGeneratorService }
import uk.gov.hmrc.gform.sdes.workitem.DestinationWorkItemAlgebra
import uk.gov.hmrc.gform.sharedmodel.{ DestinationResult, LangADT }
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.Submitted
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination.InfoArchive
import uk.gov.hmrc.gform.submission.{ InfoArchiveXMLGenerator, PdfAndXmlSummariesFactory }
import uk.gov.hmrc.gform.submission.handlebars.HandlebarsModelTree
import uk.gov.hmrc.http.HeaderCarrier

import java.security.MessageDigest
import java.time.format.DateTimeFormatter
import java.time.{ Instant, ZoneOffset }
import scala.concurrent.ExecutionContext
import scala.xml.PrettyPrinter

class InfoArchiveSubmitter(
  objectStoreAlgebra: ObjectStoreAlgebra[FOpt],
  pdfGeneratorService: PdfGeneratorService,
  fopService: FopService,
  formTemplateService: FormTemplateAlgebra[FOpt],
  destinationWorkItemAlgebra: DestinationWorkItemAlgebra[FOpt],
  formService: FormAlgebra[FOpt]
)(implicit ec: ExecutionContext)
    extends InfoArchiveSubmitterAlgebra[FOpt] {

  override def apply(
    submissionInfo: DestinationSubmissionInfo,
    modelTree: HandlebarsModelTree,
    destinationResult: Option[DestinationResult],
    d: Destination.InfoArchive,
    l: LangADT
  )(implicit hc: HeaderCarrier): FOpt[Unit] = {
    import submissionInfo._

    val pdiFileName = "eas_pdi" //Preservation Description Information
    val sipFileName = "eas_sip" //Submission Information Package

    val envelopeId = submission.envelopeId
    val formTemplateId = submission.dmsMetaData.formTemplateId
    val attachmentName = s"GFDA_${envelopeId.value}_0001"
    val prettyPrinter = new PrettyPrinter(1000, 2, minimizeEmpty = true)

    val currentTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
    val formattedCurrentTime =
      Instant.now().atOffset(ZoneOffset.UTC).format(currentTimeFormatter).replace("Z", "+00:00")

    val paths: ObjectStorePaths = ObjectStorePaths.infoArchivePaths(envelopeId)

    val sip =
      xmlDec + "\n" + prettyPrinter.formatNodes(InfoArchiveXMLGenerator.generateSip(envelopeId, formattedCurrentTime))

    for {
      formTemplate <- formTemplateService.get(formTemplateId)
      pdfContent <-
        fromFutureA(
          PdfAndXmlSummariesFactory
            .withPdf(pdfGeneratorService, fopService, modelTree.value.pdfData, modelTree.value.instructionPdfData)
            .pdfSummary(formTemplate.accessiblePdf)
            .map(_.pdfSummary.pdfContent)
        )
      _ <- uploadFile(
             paths,
             attachmentName,
             ByteString(pdfContent),
             ContentType.`application/pdf`
           )
      hash = calcHash(pdfContent)
      pdi = xmlDec + "\n" + prettyPrinter.formatNodes(
              InfoArchiveXMLGenerator.generatePdi(submission, destinationResult, attachmentName, hash)
            )
      _ <- uploadFile(
             paths,
             pdiFileName,
             ByteString(pdi),
             ContentType.`application/xml`
           )
      _ <- uploadFile(
             paths,
             sipFileName,
             ByteString(sip),
             ContentType.`application/xml`
           )
      _ <- destinationWorkItemAlgebra.pushWorkItem(envelopeId, formTemplateId, submission.submissionRef, InfoArchive)
      _ <- formService.updateFormStatus(submissionInfo.formId, Submitted)
    } yield ()
  }

  private def calcHash(content: Array[Byte]) = {
    val digest = MessageDigest.getInstance("SHA-512")
    digest.update(content)
    val checksumBytes = digest.digest()
    checksumBytes.map("%02x".format(_)).mkString
  }

  private def uploadFile(
    paths: ObjectStorePaths,
    fileName: String,
    content: ByteString,
    contentType: ContentType
  )(implicit
    hc: HeaderCarrier
  ) =
    for {
      objSummary <- objectStoreAlgebra.uploadFileWithDir(
                      paths.permanent,
                      s"$fileName.${contentType.extension}",
                      content,
                      contentType
                    )
    } yield objSummary
}
