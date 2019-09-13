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

package uk.gov.hmrc.gform.submission.destinations

import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import uk.gov.hmrc.gform.logging.Loggers
import uk.gov.hmrc.gform.sharedmodel.PdfHtml
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.submission.handlebars._
import uk.gov.hmrc.gform.wshttp.HttpResponseSyntax
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

class DestinationSubmitter[M[_], R](
  dms: DmsSubmitterAlgebra[M],
  handlebars: HandlebarsHttpApiSubmitter[M],
  stateTransitionAlgebra: StateTransitionAlgebra[M],
  destinationAuditer: Option[DestinationAuditAlgebra[M]],
  handlebarsTemplateProcessor: HandlebarsTemplateProcessor = RealHandlebarsTemplateProcessor)(
  implicit monadError: MonadError[M, String])
    extends DestinationSubmitterAlgebra[M] {

  def submitIfIncludeIf(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    submitter: DestinationsSubmitterAlgebra[M])(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    monadError.pure(
      DestinationSubmitterAlgebra
        .isIncludeIf(destination, accumulatedModel, modelTree, handlebarsTemplateProcessor)) flatMap { include =>
      if (include)
        for {
          _      <- logInfoInMonad(submissionInfo.formId, destination.id, "Included")
          result <- submit(destination, submissionInfo, accumulatedModel, modelTree, submitter)
          _      <- audit(destination, result.map(_.status), None, submissionInfo, modelTree)
        } yield result
      else
        for {
          _      <- logInfoInMonad(submissionInfo.formId, destination.id, "Not included")
          result <- Option.empty[HandlebarsDestinationResponse].pure
        } yield result

    }

  private def audit(
    destination: Destination,
    responseStatus: Option[Int],
    responseBody: Option[String],
    submissionInfo: DestinationSubmissionInfo,
    modelTree: HandlebarsModelTree)(implicit hc: HeaderCarrier): M[Unit] =
    destinationAuditer.fold(().pure[M])(
      _(
        destination,
        responseStatus,
        responseBody,
        submissionInfo.formId,
        modelTree.value.pdfData,
        submissionInfo.submission.submissionRef,
        modelTree.value.formTemplate,
        modelTree.value.model
      ))

  private def logInfoInMonad(formId: FormId, destinationId: DestinationId, msg: String): M[Unit] =
    monadError.pure {
      Loggers.destinations.info(genericLogMessage(formId, destinationId, msg))
    }

  private def submit(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    submitter: DestinationsSubmitterAlgebra[M])(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    destination match {
      case d: Destination.HmrcDms =>
        submitToDms(submissionInfo, modelTree.value.pdfData, modelTree.value.structuredFormData, d).map(_ => None)
      case d: Destination.HandlebarsHttpApi => submitToHandlebars(d, accumulatedModel, modelTree, submissionInfo)
      case d: Destination.Composite =>
        submitter
          .submitToList(DestinationList(d.destinations), submissionInfo, accumulatedModel, modelTree)
      case d: Destination.StateTransition => stateTransitionAlgebra(d, submissionInfo.formId).map(_ => None)
      case d: Destination.Log             => log(d, accumulatedModel, modelTree).map(_ => None)
    }

  def log(
    d: Destination.Log,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree): M[Unit] = {
    def modelTreeNodeString(node: HandlebarsModelTreeNode): String =
      s"${node.submissionRef}, ${node.formId.value}".padTo(100, ' ') + s": ${node.model.model}"
    def modelTreeStrings(tree: HandlebarsModelTree, depth: Int): List[String] =
      (("|  " * depth) + modelTreeNodeString(tree.value)) :: tree.children.flatMap(modelTreeStrings(_, depth + 1))

    Loggers.destinations.logger
      .info(
        s"destination: ${d.id}, accumulatedModel: ${accumulatedModel.model}, modelTree:\n${modelTreeStrings(modelTree, 1)
          .mkString("\n")}")
      .pure[M]
  }

  def submitToDms(
    submissionInfo: DestinationSubmissionInfo,
    pdfData: PdfHtml,
    structuredFormData: StructuredFormValue.ObjectStructure,
    submission: Destinations.DmsSubmission)(implicit hc: HeaderCarrier): M[Unit] =
    dms(submissionInfo, pdfData, structuredFormData, submission)

  private def submitToDms(
    submissionInfo: DestinationSubmissionInfo,
    pdfData: PdfHtml,
    structuredFormData: StructuredFormValue.ObjectStructure,
    d: Destination.HmrcDms)(implicit hc: HeaderCarrier): M[Unit] =
    monadError.handleErrorWith(submitToDms(submissionInfo, pdfData, structuredFormData, d.toDeprecatedDmsSubmission)) {
      msg =>
        if (d.failOnError)
          raiseError(submissionInfo.formId, d.id, msg)
        else {
          logInfoInMonad(submissionInfo.formId, d.id, "Failed execution but has 'failOnError' set to false. Ignoring.")
        }
    }

  private def submitToHandlebars(
    d: Destination.HandlebarsHttpApi,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    submissionInfo: DestinationSubmissionInfo)(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    handlebars(d, accumulatedModel, modelTree)
      .flatMap[HandlebarsDestinationResponse] { response =>
        if (response.isSuccess)
          createSuccessResponse(d, response)
        else if (d.failOnError)
          createFailureResponse(d, response, submissionInfo, modelTree)
        else {
          logInfoInMonad(
            submissionInfo.formId,
            d.id,
            s"Returned status code ${response.status} but has 'failOnError' set to false. Ignoring.") >>
            createSuccessResponse(d, response)
        }
      }
      .map(Option(_))

  private def createFailureResponse(
    destination: Destination.HandlebarsHttpApi,
    response: HttpResponse,
    submissionInfo: DestinationSubmissionInfo,
    modelTree: HandlebarsModelTree)(implicit hc: HeaderCarrier): M[HandlebarsDestinationResponse] =
    audit(destination, Some(response.status), Some(response.body), submissionInfo, modelTree) >>
      raiseError(
        submissionInfo.formId,
        destination.id,
        DestinationSubmitter.handlebarsHttpApiFailOnErrorMessage(response))

  private def createSuccessResponse(
    d: Destination.HandlebarsHttpApi,
    response: HttpResponse): M[HandlebarsDestinationResponse] =
    monadError.pure(HandlebarsDestinationResponse(d, response))
}

object DestinationSubmitter {
  def handlebarsHttpApiFailOnErrorMessage(response: HttpResponse): String =
    s"Returned status code ${response.status} and has 'failOnError' set to true. Failing."
}
