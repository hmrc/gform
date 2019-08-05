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

import cats.MonadError
import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.logging.Loggers
import uk.gov.hmrc.gform.sharedmodel.PdfHtml
import uk.gov.hmrc.gform.sharedmodel.form.{ FormId, FormStatus }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.submission.handlebars.{ FocussedHandlebarsModelTree, HandlebarsHttpApiSubmitter, HandlebarsModelTree, HandlebarsTemplateProcessor, RealHandlebarsTemplateProcessor }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.gform.wshttp.HttpResponseSyntax

trait DestinationSubmitter[M[_]] {
  def submitIfIncludeIf(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    submitter: DestinationsSubmitterAlgebra[M])(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]]

  def submitToDms(
    submissionInfo: DestinationSubmissionInfo,
    pdfData: PdfHtml,
    structuredFormData: StructuredFormValue.ObjectStructure,
    submission: Destinations.DmsSubmission)(implicit hc: HeaderCarrier): M[Unit]
}

class RealDestinationSubmitter[M[_], R](
  dms: DmsSubmitter[M],
  handlebars: HandlebarsHttpApiSubmitter[M],
  destinationAuditer: Option[DestinationAuditAlgebra[M]],
  formAlgebra: FormAlgebra[M],
  handlebarsTemplateProcessor: HandlebarsTemplateProcessor = RealHandlebarsTemplateProcessor)(
  implicit monadError: MonadError[M, String])
    extends DestinationSubmitter[M] {

  def submitIfIncludeIf(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    submitter: DestinationsSubmitterAlgebra[M])(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    monadError.pure(
      DestinationSubmitter.isIncludeIf(destination, accumulatedModel, modelTree, handlebarsTemplateProcessor)) flatMap {
      include =>
        if (include)
          for {
            _      <- logInfoInMonad(submissionInfo.formId, destination.id, "Included")
            result <- submit(destination, submissionInfo, accumulatedModel, modelTree, submitter)
            _      <- audit(destination, result.map(_.status), submissionInfo, modelTree.value.pdfData)
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
    submissionInfo: DestinationSubmissionInfo,
    pdfData: PdfHtml)(implicit hc: HeaderCarrier): M[Unit] =
    destinationAuditer.fold(().pure[M])(
      _(destination, responseStatus, submissionInfo.formId, pdfData, submissionInfo.submission.submissionRef))

  private def logInfoInMonad(formId: FormId, destinationId: DestinationId, msg: String): M[Unit] =
    monadError.pure {
      Loggers.destinations.info(RealDestinationSubmitter.genericLogMessage(formId, destinationId, msg))
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
      case d: Destination.StateTransition => transitionState(d, submissionInfo.formId).map(_ => None)
    }

  def transitionState(d: Destination.StateTransition, formId: FormId)(implicit hc: HeaderCarrier): M[Unit] =
    formAlgebra
      .updateFormStatus(formId, d.requiredState)
      .flatMap { stateAchieved =>
        if (stateAchieved === d.requiredState || !d.failOnError) monadError.pure(())
        else raiseError(formId, d.id, RealDestinationSubmitter.stateTransitionFailOnErrorMessage(d, stateAchieved))
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
          createFailureResponse(d, response, submissionInfo, modelTree.value.pdfData)
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
    pdfHtml: PdfHtml)(implicit hc: HeaderCarrier): M[HandlebarsDestinationResponse] =
    audit(destination, Some(response.status), submissionInfo, pdfHtml) >>
      raiseError(
        submissionInfo.formId,
        destination.id,
        RealDestinationSubmitter.handlebarsHttpApiFailOnErrorMessage(response))

  private def createSuccessResponse(
    d: Destination.HandlebarsHttpApi,
    response: HttpResponse): M[HandlebarsDestinationResponse] =
    monadError.pure(HandlebarsDestinationResponse(d, response))

  protected def raiseError[T](formId: FormId, destinationId: DestinationId, msg: String) = {
    val fullMsg = RealDestinationSubmitter.genericLogMessage(formId, destinationId, msg)
    monadError.pure { Loggers.destinations.warn(fullMsg) } >>
      monadError.raiseError[T](fullMsg)
  }
}

object DestinationSubmitter {
  def isIncludeIf(
    destination: Destination,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    handlebarsTemplateProcessor: HandlebarsTemplateProcessor): Boolean =
    handlebarsTemplateProcessor(
      destination.includeIf,
      accumulatedModel,
      FocussedHandlebarsModelTree(modelTree, modelTree.value.model),
      TemplateType.Plain) === true.toString
}

object RealDestinationSubmitter {
  def handlebarsHttpApiFailOnErrorMessage(response: HttpResponse): String =
    s"Returned status code ${response.status} and has 'failOnError' set to true. Failing."

  def stateTransitionFailOnErrorMessage(d: Destination.StateTransition, currentState: FormStatus): String =
    s"Cannot achieve transition from $currentState to ${d.requiredState}"

  def genericLogMessage(formId: FormId, destinationId: DestinationId, msg: String): String =
    f"${formId.value}%-60s ${destinationId.id}%-30s $msg"
}
