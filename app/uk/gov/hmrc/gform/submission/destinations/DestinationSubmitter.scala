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

import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import uk.gov.hmrc.gform.logging.Loggers
import uk.gov.hmrc.gform.notifier.NotifierAlgebra
import uk.gov.hmrc.gform.sharedmodel.{ EmailVerifierService, LangADT, PdfHtml }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormData, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.submission.handlebars._
import uk.gov.hmrc.gform.submissionconsolidator.SubmissionConsolidatorAlgebra
import uk.gov.hmrc.gform.wshttp.HttpResponseSyntax
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

class DestinationSubmitter[M[_]](
  dms: DmsSubmitterAlgebra[M],
  handlebars: HandlebarsHttpApiSubmitter[M],
  stateTransitionAlgebra: StateTransitionAlgebra[M],
  notifier: NotifierAlgebra[M],
  destinationAuditer: Option[DestinationAuditAlgebra[M]],
  submissionConsolidator: SubmissionConsolidatorAlgebra[M],
  handlebarsTemplateProcessor: HandlebarsTemplateProcessor = RealHandlebarsTemplateProcessor
)(implicit monadError: MonadError[M, String])
    extends DestinationSubmitterAlgebra[M] {

  def submitIfIncludeIf(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    submitter: DestinationsSubmitterAlgebra[M],
    formData: Option[FormData],
    l: LangADT
  )(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    monadError.pure(
      DestinationSubmitterAlgebra
        .isIncludeIf(destination, accumulatedModel, modelTree, handlebarsTemplateProcessor)
    ) flatMap { include =>
      if (include)
        for {
          _      <- logInfoInMonad(submissionInfo.formId, destination.id, "Included")
          result <- submit(destination, submissionInfo, accumulatedModel, modelTree, submitter, formData, l)
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
    modelTree: HandlebarsModelTree
  )(implicit hc: HeaderCarrier): M[Unit] =
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
      )
    )

  private def logInfoInMonad(formId: FormId, destinationId: DestinationId, msg: String): M[Unit] =
    monadError.pure {
      Loggers.destinations.info(genericLogMessage(formId, destinationId, msg))
    }

  private def logErrorInMonad(formId: FormId, destinationId: DestinationId, msg: String): M[Unit] =
    monadError.pure {
      Loggers.destinations.error(genericLogMessage(formId, destinationId, msg))
    }

  private def submit(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    submitter: DestinationsSubmitterAlgebra[M],
    formData: Option[FormData],
    l: LangADT
  )(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    destination match {
      case d: Destination.HmrcDms =>
        submitToDms(
          submissionInfo,
          modelTree.value.pdfData,
          modelTree.value.instructionPdfData,
          modelTree.value.structuredFormData,
          d
        ).map(_ => None)
      case d: Destination.HandlebarsHttpApi => submitToHandlebars(d, accumulatedModel, modelTree, submissionInfo)
      case d: Destination.Composite =>
        submitter
          .submitToList(d.destinations, submissionInfo, accumulatedModel, modelTree, formData, l)
      case d: Destination.StateTransition => stateTransitionAlgebra(d, submissionInfo.formId).map(_ => None)
      case d: Destination.Log             => log(d, accumulatedModel, modelTree).map(_ => None)
      case d: Destination.Email =>
        submitToEmail(d, submissionInfo, modelTree.value.structuredFormData, l).map(_ => None)
      case d: Destination.SubmissionConsolidator =>
        submitToSubmissionConsolidator(d, submissionInfo, accumulatedModel, modelTree, formData).map(_ => None)
    }

  def submitToSubmissionConsolidator(
    d: Destination.SubmissionConsolidator,
    submissionInfo: DestinationSubmissionInfo,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    formData: Option[FormData]
  )(implicit hc: HeaderCarrier): M[Unit] =
    monadError.handleErrorWith(
      submissionConsolidator.submit(d, submissionInfo, accumulatedModel, modelTree, formData)
    ) { msg =>
      if (d.failOnError)
        raiseError(submissionInfo.formId, d.id, msg)
      else {
        logInfoInMonad(submissionInfo.formId, d.id, "Failed execution but has 'failOnError' set to false. Ignoring.")
      }
    }

  def submitToEmail(
    d: Destination.Email,
    submissionInfo: DestinationSubmissionInfo,
    structuredFormData: StructuredFormValue.ObjectStructure,
    l: LangADT
  ): M[Unit] =
    d.emailVerifierService match {
      case notify @ EmailVerifierService.Notify(_, _) =>
        submitToNotify(notify, d, submissionInfo, structuredFormData, l)
      case EmailVerifierService.DigitalContact(_, _) =>
        raiseError(submissionInfo.formId, d.id, "DigitalContact destination support is not implemented")
    }

  private def submitToNotify(
    notify: EmailVerifierService.Notify,
    d: Destination.Email,
    submissionInfo: DestinationSubmissionInfo,
    structuredFormData: StructuredFormValue.ObjectStructure,
    l: LangADT
  ): M[Unit] =
    monadError.handleErrorWith(
      NotifierEmailBuilder(notify.notifierTemplateId(l), d, structuredFormData) >>=
        notifier.email
    ) { msg =>
      if (d.failOnError)
        raiseError(submissionInfo.formId, d.id, msg)
      else {
        logInfoInMonad(submissionInfo.formId, d.id, "Failed execution but has 'failOnError' set to false. Ignoring.")
      }
    }

  def log(
    d: Destination.Log,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree
  ): M[Unit] = {
    def modelTreeNodeString(node: HandlebarsModelTreeNode): String =
      s"${node.submissionRef}, ${node.formId.value}".padTo(100, ' ') + s": ${node.model.model}"
    def modelTreeStrings(tree: HandlebarsModelTree, depth: Int): List[String] =
      (("|  " * depth) + modelTreeNodeString(tree.value)) :: tree.children.flatMap(modelTreeStrings(_, depth + 1))

    Loggers.destinations.logger
      .info(
        s"destination: ${d.id}, accumulatedModel: ${accumulatedModel.model}, modelTree:\n${modelTreeStrings(modelTree, 1)
          .mkString("\n")}"
      )
      .pure[M]
  }

  def submitToDms(
    submissionInfo: DestinationSubmissionInfo,
    pdfData: PdfHtml,
    instructionPdfData: Option[PdfHtml],
    structuredFormData: StructuredFormValue.ObjectStructure,
    d: Destination.HmrcDms
  )(implicit hc: HeaderCarrier): M[Unit] =
    monadError.handleErrorWith(dms(submissionInfo, pdfData, instructionPdfData, structuredFormData, d)) { msg =>
      if (d.failOnError)
        raiseError(submissionInfo.formId, d.id, msg)
      else {
        logErrorInMonad(submissionInfo.formId, d.id, "Failed execution but has 'failOnError' set to false. Ignoring.")
      }
    }

  private def submitToHandlebars(
    d: Destination.HandlebarsHttpApi,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    submissionInfo: DestinationSubmissionInfo
  )(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    handlebars(d, accumulatedModel, modelTree)
      .flatMap[HandlebarsDestinationResponse] { response =>
        if (response.isSuccess)
          createSuccessResponse(d, response)
        else if (d.failOnError)
          createFailureResponse(d, response, submissionInfo, modelTree)
        else {
          logErrorInMonad(
            submissionInfo.formId,
            d.id,
            s"Returned status code ${response.status} but has 'failOnError' set to false. Ignoring."
          ) >>
            createSuccessResponse(d, response)
        }
      }
      .map(Option(_))

  private def createFailureResponse(
    destination: Destination.HandlebarsHttpApi,
    response: HttpResponse,
    submissionInfo: DestinationSubmissionInfo,
    modelTree: HandlebarsModelTree
  )(implicit hc: HeaderCarrier): M[HandlebarsDestinationResponse] =
    audit(destination, Some(response.status), Some(response.body), submissionInfo, modelTree) >>
      raiseError(
        submissionInfo.formId,
        destination.id,
        DestinationSubmitter.handlebarsHttpApiFailOnErrorMessage(response)
      )

  private def createSuccessResponse(
    d: Destination.HandlebarsHttpApi,
    response: HttpResponse
  ): M[HandlebarsDestinationResponse] =
    monadError.pure(HandlebarsDestinationResponse(d, response))
}

object DestinationSubmitter {
  def handlebarsHttpApiFailOnErrorMessage(response: HttpResponse): String =
    s"Returned status code ${response.status} and has 'failOnError' set to true. Failing."
}
