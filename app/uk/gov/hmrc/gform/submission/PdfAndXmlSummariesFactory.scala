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

package uk.gov.hmrc.gform.submission

import org.apache.pdfbox.pdmodel.PDDocument
import org.json4s.native.JsonMethods
import org.json4s.native.Printer.compact
import uk.gov.hmrc.gform.pdfgenerator.{ PdfGeneratorService, XmlGeneratorService }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DataOutputFormat
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, PdfHtml, SubmissionRef }

import java.time.Instant
import scala.concurrent.{ ExecutionContext, Future }

trait PdfAndXmlSummariesFactory {
  def apply(
    form: Form,
    formTemplate: FormTemplate,
    structuredFormData: StructuredFormValue.ObjectStructure,
    customerId: String,
    submissionRef: SubmissionRef,
    hmrcDms: HmrcDms,
    l: LangADT
  )(implicit now: Instant): Future[PdfAndXmlSummaries]
}

object PdfAndXmlSummariesFactory {

  def withPdf(pdfGeneratorService: PdfGeneratorService, pdfData: PdfHtml, instructionPdfData: Option[PdfHtml])(implicit
    ec: ExecutionContext
  ): PdfAndXmlSummariesFactory = new PdfAndXmlSummariesFactory {

    override def apply(
      form: Form,
      formTemplate: FormTemplate,
      structuredFormData: StructuredFormValue.ObjectStructure,
      customerId: String,
      submissionRef: SubmissionRef,
      hmrcDms: HmrcDms,
      l: LangADT
    )(implicit now: Instant): Future[PdfAndXmlSummaries] =
      for {
        pdf <- pdfGeneratorService.generatePDFBytesLocal(pdfData.html.replaceAllLiterally("<br>", "<br/>"))
        instructionPdf <- instructionPdfData.fold(Future.successful(Option.empty[Array[Byte]]))(iPdfData =>
                            pdfGeneratorService.generatePDFBytesLocal(iPdfData.html).map(Option(_))
                          )
      } yield PdfAndXmlSummaries(
        pdfSummary = createPdfSummary(pdf),
        instructionPdfSummary = instructionPdf.map(createPdfSummary),
        roboticsFile =
          createRoboticsFile(formTemplate, structuredFormData, hmrcDms, submissionRef, l, Some(form.envelopeId)),
        roboticsFileExtension = hmrcDms.dataOutputFormat.map(_.content),
        formDataXml = createFormdataXml(formTemplate, structuredFormData, hmrcDms, submissionRef, l, None)
      )

    private def createPdfSummary(pdf: Array[Byte]) = {
      val pDDocument: PDDocument = PDDocument.load(pdf)
      try PdfSummary(numberOfPages = pDDocument.getNumberOfPages.toLong, pdfContent = pdf)
      finally pDDocument.close()
    }

    private def createFormdataXml(
      formTemplate: FormTemplate,
      structuredFormData: StructuredFormValue.ObjectStructure,
      hmrcDms: HmrcDms,
      submissionRef: SubmissionRef,
      l: LangADT,
      envelopeId: Option[EnvelopeId]
    )(implicit now: Instant): Option[String] =
      if (hmrcDms.formdataXml)
        generateRoboticsFile(
          formTemplate,
          structuredFormData,
          hmrcDms,
          submissionRef,
          Some(DataOutputFormat.XML),
          l,
          envelopeId
        )
      else None

    private def createRoboticsFile(
      formTemplate: FormTemplate,
      structuredFormData: StructuredFormValue.ObjectStructure,
      hmrcDms: HmrcDms,
      submissionRef: SubmissionRef,
      l: LangADT,
      envelopeId: Option[EnvelopeId]
    )(implicit now: Instant): Option[String] =
      generateRoboticsFile(
        formTemplate,
        structuredFormData,
        hmrcDms,
        submissionRef,
        hmrcDms.dataOutputFormat,
        l,
        envelopeId
      )

    private def generateRoboticsFile(
      formTemplate: FormTemplate,
      structuredFormData: StructuredFormValue.ObjectStructure,
      hmrcDms: HmrcDms,
      submissionRef: SubmissionRef,
      dataOutputFormat: Option[DataOutputFormat],
      l: LangADT,
      envelopeId: Option[EnvelopeId]
    )(implicit now: Instant): Option[String] =
      dataOutputFormat.flatMap {
        case DataOutputFormat.XML =>
          Some(
            RoboticsXMLGenerator(
              formTemplate._id,
              hmrcDms.dmsFormId,
              submissionRef,
              structuredFormData,
              now,
              l,
              envelopeId,
              sanitizeRequired = true
            )
          ).map { body =>
            // No whitespace of anysort after xml declaration. Robot won't be able to process xml otherwise
            val xml = XmlGeneratorService.xmlDec + <data xmlns:xfa="http://www.xfa.org/schema/xfa-data/1.0/">
              {body}
            </data>
            org.apache.commons.text.StringEscapeUtils.unescapeXml(xml)
          }
        case DataOutputFormat.JSON =>
          Some(
            RoboticsXMLGenerator(
              formTemplate._id,
              hmrcDms.dmsFormId,
              submissionRef,
              structuredFormData,
              now,
              l,
              envelopeId,
              sanitizeRequired = false
            )
          )
            .map(xml => compact(JsonMethods.render(org.json4s.Xml.toJson(xml))))
      }
  }
}
