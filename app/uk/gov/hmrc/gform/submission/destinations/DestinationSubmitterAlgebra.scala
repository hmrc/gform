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

package uk.gov.hmrc.gform.submission.destinations

import cats.instances.string._
import cats.syntax.eq._
import uk.gov.hmrc.gform.sharedmodel.{ FrontEndSubmissionDesIncludeIfEval, LangADT, PdfHtml }
import uk.gov.hmrc.gform.sharedmodel.form.FormData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.submission.handlebars._
import uk.gov.hmrc.http.HeaderCarrier

trait DestinationSubmitterAlgebra[M[_]] {
  def submitIfIncludeIf(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    submitter: DestinationsSubmitterAlgebra[M],
    formData: Option[FormData],
    l: LangADT,
    desIncludeIfEval: FrontEndSubmissionDesIncludeIfEval
  )(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]]

  def submitToDms(
    submissionInfo: DestinationSubmissionInfo,
    pdfData: PdfHtml,
    instructionPdfData: Option[PdfHtml],
    structuredFormData: StructuredFormValue.ObjectStructure,
    hmrcDms: HmrcDms,
    l: LangADT
  )(implicit hc: HeaderCarrier): M[Unit]
}

object DestinationSubmitterAlgebra {
  def isIncludeIf(
    destination: Destination,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    handlebarsTemplateProcessor: HandlebarsTemplateProcessor,
    desIncludeIfEval: FrontEndSubmissionDesIncludeIfEval
  ): Boolean =
    destination.includeIf match {
      case DestinationIncludeIf.StringValue(value) =>
        handlebarsTemplateProcessor(
          value,
          accumulatedModel,
          FocussedHandlebarsModelTree(modelTree, modelTree.value.model),
          TemplateType.Plain
        ) === true.toString
      case DestinationIncludeIf.IncludeIfValue(_) =>
        desIncludeIfEval.exprEval.find(_._1 === destination.id).map(_._2).getOrElse(false)
    }

}
