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

package uk.gov.hmrc.gform.submission

import java.time.Instant
import org.apache.pdfbox.pdmodel.PDDocument
import uk.gov.hmrc.gform.pdfgenerator.{ PdfGeneratorService, XmlGeneratorService }
import uk.gov.hmrc.gform.sharedmodel.{ PdfHtml, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, SectionFormField }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DmsSubmission
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }
import scala.xml.NodeSeq

trait PdfAndXmlSummariesFactory {
  def apply(
    form: Form,
    formTemplate: FormTemplate,
    structuredFormData: StructuredFormValue.ObjectStructure,
    sectionFormFields: List[SectionFormField],
    customerId: String,
    submissionRef: SubmissionRef,
    dmsSubmission: DmsSubmission): Future[PdfAndXmlSummaries]
}

object PdfAndXmlSummariesFactory {

  def withPdf(pdfGeneratorService: PdfGeneratorService, pdfData: PdfHtml)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): PdfAndXmlSummariesFactory = new PdfAndXmlSummariesFactory {

    override def apply(
      form: Form,
      formTemplate: FormTemplate,
      structuredFormData: StructuredFormValue.ObjectStructure,
      sectionFormFields: List[SectionFormField],
      customerId: String,
      submissionRef: SubmissionRef,
      dmsSubmission: DmsSubmission): Future[PdfAndXmlSummaries] =
      pdfGeneratorService.generatePDFBytes(pdfData.html).map { pdf =>
        PdfAndXmlSummaries(
          pdfSummary = createPdfSummary(pdf),
          xmlSummary = createXmlSummary(sectionFormFields, formTemplate, submissionRef, dmsSubmission),
          roboticsXml = createRoboticsXml(formTemplate, form, structuredFormData, dmsSubmission, submissionRef)
        )
      }

    private def createPdfSummary(pdf: Array[Byte]) = {
      val pDDocument: PDDocument = PDDocument.load(pdf)
      try {
        PdfSummary(numberOfPages = pDDocument.getNumberOfPages.toLong, pdfContent = pdf)
      } finally {
        pDDocument.close()
      }
    }

    private def createXmlSummary(
      sectionFormFields: List[SectionFormField],
      formTemplate: FormTemplate,
      submissionRef: SubmissionRef,
      dmsSubmission: Destinations.DmsSubmission) = {
      val xmlSummary = dmsSubmission.dataXml
        .filter(identity)
        .map(_ => XmlGeneratorService.getXml(sectionFormFields, submissionRef))
        .map(addXmlDeclaration)
      xmlSummary
    }

    private def createRoboticsXml(
      formTemplate: FormTemplate,
      form: Form,
      structuredFormData: StructuredFormValue.ObjectStructure,
      dmsSubmission: DmsSubmission,
      submissionRef: SubmissionRef): Option[String] =
      dmsSubmission.includeRoboticsXml
        .filter(identity)
        .map(
          _ =>
            RoboticsXMLGenerator(
              formTemplate._id,
              dmsSubmission.dmsFormId,
              submissionRef,
              structuredFormData,
              Instant.now()))
        .map(body =>
          // No whitespace of anysort after xml declaration. Robot won't be able to process xml otherwise
          XmlGeneratorService.xmlDec + <data xmlns:xfa="http://www.xfa.org/schema/xfa-data/1.0/">{body}</data>)

  }

  private def addXmlDeclaration(xml: NodeSeq) =
    XmlGeneratorService.xmlDec + "\n" + xml
}
