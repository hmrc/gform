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

package uk.gov.hmrc.gform.submission.handlebars

import uk.gov.hmrc.gform.sharedmodel.{ PdfContent, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.HandlebarsTemplateProcessorModel
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue

case class HandlebarsModelTreeNode(
  formId: FormId,
  submissionRef: SubmissionRef,
  formTemplate: FormTemplate,
  model: HandlebarsTemplateProcessorModel,
  pdfData: PdfContent,
  instructionPdfData: Option[PdfContent],
  structuredFormData: StructuredFormValue.ObjectStructure
)
