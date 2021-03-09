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

import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.{ PdfHtml, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.HandlebarsTemplateProcessorModel
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue

package object handlebars {
  type HandlebarsModelTree = Tree[HandlebarsModelTreeNode]

  object HandlebarsModelTree {
    def apply(
      formId: FormId,
      submissionRef: SubmissionRef,
      formTemplate: FormTemplate,
      pdfData: PdfHtml,
      instructionPdfData: Option[PdfHtml],
      structuredFormData: StructuredFormValue.ObjectStructure,
      model: HandlebarsTemplateProcessorModel,
      children: HandlebarsModelTree*
    ): HandlebarsModelTree =
      Tree(
        HandlebarsModelTreeNode(
          formId,
          submissionRef,
          formTemplate,
          model,
          pdfData,
          instructionPdfData,
          structuredFormData
        ),
        children: _*
      )
  }
}
