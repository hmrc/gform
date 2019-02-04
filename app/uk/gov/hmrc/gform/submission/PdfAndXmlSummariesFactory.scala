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

import cats.instances.future._
import org.apache.pdfbox.pdmodel.PDDocument
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.pdfgenerator.{ HtmlGeneratorService, PdfGeneratorService, XmlGeneratorService }
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DmsSubmission
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext

trait PdfAndXmlSummariesFactory {
  def apply(
    form: Form,
    formTemplate: FormTemplate,
    sectionFormFields: List[SectionFormField],
    customerId: String,
    submissionRef: SubmissionRef,
    dmsSubmission: DmsSubmission): FOpt[PdfAndXmlSummaries]
}

object PdfAndXmlSummariesFactory {
  def withoutPdf(pdfGeneratorService: PdfGeneratorService)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): PdfAndXmlSummariesFactory = new PdfAndXmlSummariesFactory {
    override def apply(
      form: Form,
      formTemplate: FormTemplate,
      sectionFormFields: List[SectionFormField],
      customerId: String,
      submissionRef: SubmissionRef,
      dmsSubmission: DmsSubmission): FOpt[PdfAndXmlSummaries] = {
      val pdf = HtmlGeneratorService.generateDocumentHTML(sectionFormFields, formTemplate.formName, form.formData)
      val wpdf: PdfAndXmlSummariesFactory = withPdf(pdfGeneratorService, pdf)
      wpdf(form, formTemplate, sectionFormFields, customerId, submissionRef, dmsSubmission)
    }
  }

  def withPdf(pdfGeneratorService: PdfGeneratorService, pdfHtml: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): PdfAndXmlSummariesFactory = new PdfAndXmlSummariesFactory {
    override def apply(
      form: Form,
      formTemplate: FormTemplate,
      sectionFormFields: List[SectionFormField],
      customerId: String,
      submissionRef: SubmissionRef,
      dmsSubmission: DmsSubmission): FOpt[PdfAndXmlSummaries] =
      pdfGeneratorService.generatePDF(pdfHtml).map { pdf =>
        PdfAndXmlSummaries(
          pdfSummary = createPdfSummary(pdf),
          xmlSummary = createXmlSummary(sectionFormFields, formTemplate, submissionRef, dmsSubmission))
      }

    private def createPdfSummary(pdf: Array[Byte]): PdfSummary = {
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
      val xmlSummary = dmsSubmission.dataXml match {
        case Some(true) =>
          Some(XmlGeneratorService.xmlDec + "\n" + XmlGeneratorService.getXml(sectionFormFields, submissionRef))
        case _ => None

      }
      xmlSummary
    }
  }
}
