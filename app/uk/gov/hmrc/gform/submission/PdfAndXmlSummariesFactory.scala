/*
 * Copyright 2021 HM Revenue & Customs
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

import java.time.Instant

import org.apache.pdfbox.pdmodel.PDDocument
import uk.gov.hmrc.gform.pdfgenerator.{ PdfGeneratorService, XmlGeneratorService }
import uk.gov.hmrc.gform.sharedmodel.{ PdfHtml, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

trait PdfAndXmlSummariesFactory {
  def apply(
    form: Form,
    formTemplate: FormTemplate,
    structuredFormData: StructuredFormValue.ObjectStructure,
    customerId: String,
    submissionRef: SubmissionRef,
    hmrcDms: HmrcDms
  )(implicit now: Instant): Future[PdfAndXmlSummaries]
}

object PdfAndXmlSummariesFactory {

  def withPdf(pdfGeneratorService: PdfGeneratorService, pdfData: PdfHtml, instructionPdfData: Option[PdfHtml])(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): PdfAndXmlSummariesFactory = new PdfAndXmlSummariesFactory {

    override def apply(
      form: Form,
      formTemplate: FormTemplate,
      structuredFormData: StructuredFormValue.ObjectStructure,
      customerId: String,
      submissionRef: SubmissionRef,
      hmrcDms: HmrcDms
    )(implicit now: Instant): Future[PdfAndXmlSummaries] =
      for {
        pdf <- pdfGeneratorService.generatePDFBytes(pdfData.html)
        instructionPdf <- instructionPdfData.fold(Future.successful(Option.empty[Array[Byte]]))(iPdfData =>
                            pdfGeneratorService.generatePDFBytesLocal(iPdfData.html).map(Option(_))
                          )
      } yield PdfAndXmlSummaries(
        pdfSummary = createPdfSummary(pdf),
        instructionPdfSummary = instructionPdf.map(createPdfSummary),
        roboticsXml = createRoboticsXml(formTemplate, structuredFormData, hmrcDms, submissionRef),
        formDataXml = createFormdataXml(formTemplate, structuredFormData, hmrcDms, submissionRef)
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
      submissionRef: SubmissionRef
    )(implicit now: Instant): Option[String] =
      generateRoboticsXml(formTemplate, structuredFormData, hmrcDms, submissionRef, _.formdataXml)

    private def createRoboticsXml(
      formTemplate: FormTemplate,
      structuredFormData: StructuredFormValue.ObjectStructure,
      hmrcDms: HmrcDms,
      submissionRef: SubmissionRef
    )(implicit now: Instant): Option[String] =
      generateRoboticsXml(formTemplate, structuredFormData, hmrcDms, submissionRef, _.roboticsXml)

    private def generateRoboticsXml(
      formTemplate: FormTemplate,
      structuredFormData: StructuredFormValue.ObjectStructure,
      hmrcDms: HmrcDms,
      submissionRef: SubmissionRef,
      condition: HmrcDms => Boolean
    )(implicit now: Instant) =
      if (condition(hmrcDms)) {
        Some(RoboticsXMLGenerator(formTemplate._id, hmrcDms.dmsFormId, submissionRef, structuredFormData, now))
          .map(body =>
            // No whitespace of anysort after xml declaration. Robot won't be able to process xml otherwise
            XmlGeneratorService.xmlDec + <data xmlns:xfa="http://www.xfa.org/schema/xfa-data/1.0/">
                {body}
              </data>
          )
      } else {
        None
      }
  }
}
