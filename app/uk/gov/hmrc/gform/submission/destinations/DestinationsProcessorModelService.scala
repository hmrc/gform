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

package uk.gov.hmrc.gform.submission.destinations

import cats.Monad
import cats.instances.option._
import cats.syntax.functor._
import cats.syntax.traverse._
import uk.gov.hmrc.gform.fileupload.{ FileDownloadAlgebra, UploadedFile }
import uk.gov.hmrc.gform.sharedmodel.{ FrontEndSubmissionVariables, PdfHtml }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.HandlebarsTemplateProcessorModel
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.http.HeaderCarrier

class DestinationsProcessorModelService[M[_]: Monad](fileDownloadAlgebra: Option[FileDownloadAlgebra[M]])
    extends DestinationsProcessorModelAlgebra[M] {
  override def create(
    form: Form,
    frontEndSubmissionVariables: FrontEndSubmissionVariables,
    pdfData: PdfHtml,
    instructionPdfData: Option[PdfHtml],
    structuredFormData: StructuredFormValue.ObjectStructure)(
    implicit hc: HeaderCarrier): M[HandlebarsTemplateProcessorModel] =
    for {
      files <- uploadedFiles(form.envelopeId)
    } yield
      DestinationsProcessorModelAlgebra
        .createModel(frontEndSubmissionVariables, pdfData, instructionPdfData, structuredFormData, form, files)

  private def uploadedFiles(envelopedId: EnvelopeId)(implicit hc: HeaderCarrier): M[Option[List[UploadedFile]]] =
    fileDownloadAlgebra.traverse { _.allUploadedFiles(envelopedId) }
}
