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

import cats.syntax.eq._
import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.slf4j.LoggerFactory
import play.api.libs.json._
import uk.gov.hmrc.gform.notifier.NotifierAlgebra
import uk.gov.hmrc.gform.sdes.SdesConfig
import uk.gov.hmrc.gform.sharedmodel.{ DestinationEvaluation, DestinationResult, EmailVerifierService, LangADT, UserSession }
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
  dataStore: DataStoreSubmitterAlgebra[M],
  infoArchive: InfoArchiveSubmitterAlgebra[M],
  sdesConfig: SdesConfig,
  handlebarsTemplateProcessor: HandlebarsTemplateProcessor = RealHandlebarsTemplateProcessor
)(implicit monadError: MonadError[M, Throwable])
    extends DestinationSubmitterAlgebra[M] {

  private val logger = LoggerFactory.getLogger(getClass)

  def submitIfIncludeIf(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    submitter: DestinationsSubmitterAlgebra[M],
    formData: Option[FormData],
    l: LangADT,
    destinationEvaluation: DestinationEvaluation,
    userSession: UserSession
  )(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    monadError.pure(
      DestinationSubmitterAlgebra
        .isIncludeIf(destination, accumulatedModel, modelTree, handlebarsTemplateProcessor, destinationEvaluation)
    ) flatMap { include =>
      if (include)
        for {
          _ <- logInfoInMonad(submissionInfo.formId, destination.id, "Included")
          maybeCustomerId =
            destinationEvaluation.evaluation
              .find(_.destinationId === destination.id)
              .flatMap(_.customerId)
          result <-
            submit(
              destination,
              maybeCustomerId.fold(submissionInfo)(customerId =>
                submissionInfo.copy(submission =
                  submissionInfo.submission
                    .copy(dmsMetaData = submissionInfo.submission.dmsMetaData.copy(customerId = customerId))
                )
              ),
              accumulatedModel,
              modelTree,
              submitter,
              formData,
              l,
              destinationEvaluation,
              userSession
            )
          _ <- audit(destination, result.map(_.status), None, submissionInfo, modelTree)
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
      logger.info(genericLogMessage(formId, destinationId, msg))
    }

  private def logErrorInMonad(formId: FormId, destinationId: DestinationId, msg: String): M[Unit] =
    monadError.pure {
      logger.error(genericLogMessage(formId, destinationId, msg))
    }

  private def submit(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    submitter: DestinationsSubmitterAlgebra[M],
    formData: Option[FormData],
    l: LangADT,
    destinationEvaluation: DestinationEvaluation,
    userSession: UserSession
  )(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    destination match {
      case d: Destination.HmrcDms =>
        submitToDms(
          submissionInfo,
          accumulatedModel,
          modelTree,
          d,
          l
        ).map(_ => None)
      case d: Destination.DataStore =>
        submitToDataStore(
          submissionInfo,
          modelTree.value.structuredFormData,
          d,
          l,
          userSession,
          destinationEvaluation.evaluation.find(_.destinationId === d.id),
          accumulatedModel,
          modelTree
        ).map(_ => None)
      case d: Destination.InfoArchive =>
        submitToInfoArchive(
          submissionInfo,
          modelTree,
          destinationEvaluation.evaluation.find(_.destinationId === d.id),
          d,
          l
        ).map(_ => None)
      case d: Destination.HandlebarsHttpApi => submitToHandlebars(d, accumulatedModel, modelTree, submissionInfo)
      case d: Destination.Composite =>
        submitter
          .submitToList(
            d.destinations,
            submissionInfo,
            accumulatedModel,
            modelTree,
            formData,
            l,
            destinationEvaluation,
            userSession
          )
      case d: Destination.StateTransition => stateTransitionAlgebra(d, submissionInfo.formId).map(_ => None)
      case d: Destination.Log             => log(d, accumulatedModel, modelTree).map(_ => None)
      case d: Destination.Email =>
        submitToEmail(d, submissionInfo, modelTree.value.structuredFormData, l).map(_ => None)
      case d: Destination.SubmissionConsolidator =>
        submitToSubmissionConsolidator(d, submissionInfo, accumulatedModel, modelTree, formData).map(_ => None)
    }

  private def submitToSubmissionConsolidator(
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
        raiseDestinationError(submissionInfo.formId, d.id, msg)
      else {
        logInfoInMonad(submissionInfo.formId, d.id, "Failed execution but has 'failOnError' set to false. Ignoring.")
      }
    }

  private def submitToEmail(
    d: Destination.Email,
    submissionInfo: DestinationSubmissionInfo,
    structuredFormData: StructuredFormValue.ObjectStructure,
    l: LangADT
  ): M[Unit] =
    d.emailVerifierService match {
      case notify @ EmailVerifierService.Notify(_, _) =>
        submitToNotify(notify, d, submissionInfo, structuredFormData, l)
      case EmailVerifierService.DigitalContact(_, _) =>
        raiseDestinationError(
          submissionInfo.formId,
          d.id,
          new Exception("DigitalContact destination support is not implemented")
        )
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
        raiseDestinationError(submissionInfo.formId, d.id, msg)
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

    logger
      .info(
        s"destination: ${d.id}, accumulatedModel: ${accumulatedModel.model}, modelTree:\n${modelTreeStrings(modelTree, 1)
          .mkString("\n")}"
      )
      .pure[M]
  }

  def submitToDms(
    submissionInfo: DestinationSubmissionInfo,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    d: Destination.HmrcDms,
    l: LangADT
  )(implicit hc: HeaderCarrier): M[Unit] =
    monadError.handleErrorWith(dms(submissionInfo, accumulatedModel, modelTree, d, l)) { msg =>
      if (d.failOnError)
        raiseDestinationError(submissionInfo.formId, d.id, msg)
      else {
        logErrorInMonad(submissionInfo.formId, d.id, "Failed execution but has 'failOnError' set to false. Ignoring.")
      }
    }

  private def submitToDataStore(
    submissionInfo: DestinationSubmissionInfo,
    structuredFormData: StructuredFormValue.ObjectStructure,
    d: Destination.DataStore,
    l: LangADT,
    userSession: UserSession,
    destinationResult: Option[DestinationResult],
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree
  ): M[Unit] = {
    val payload = dataStore.generatePayload(
      submissionInfo,
      structuredFormData,
      d,
      l,
      userSession,
      destinationResult.flatMap(_.taxpayerId),
      accumulatedModel,
      modelTree
    )
    dataStore.validateSchema(d, payload) match {
      case Left(message) =>
        val payloadWithoutPII =
          try {
            val json = Json.parse(payload)
            Json.prettyPrint(replaceStringsWithLengths(json))
          } catch {
            case e: Exception => s"Failed to parse JSON payload: ${e.getMessage}"
          }
        throw new RuntimeException(
          s"Schema validation failed for submission id '${submissionInfo.formId.value}': $message \nPayload: $payloadWithoutPII"
        )
      case _ =>
        val dataStoreRouting = d.routing.sdesRouting(sdesConfig)
        monadError.handleErrorWith(
          dataStore.submitPayload(
            submissionInfo,
            payload,
            dataStoreRouting,
            d.routing
          )
        ) { msg =>
          if (d.failOnError)
            raiseDestinationError(submissionInfo.formId, d.id, msg)
          else {
            logErrorInMonad(
              submissionInfo.formId,
              d.id,
              "Failed execution but has 'failOnError' set to false. Ignoring."
            )
          }
        }
    }
  }

  def submitToInfoArchive(
    submissionInfo: DestinationSubmissionInfo,
    modelTree: HandlebarsModelTree,
    destinationResult: Option[DestinationResult],
    d: Destination.InfoArchive,
    l: LangADT
  )(implicit hc: HeaderCarrier): M[Unit] =
    monadError.handleErrorWith(infoArchive(submissionInfo, modelTree, destinationResult, d, l)) { msg =>
      if (d.failOnError)
        raiseDestinationError(submissionInfo.formId, d.id, msg)
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
    handlebars(d, accumulatedModel, modelTree, submissionInfo)
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
      raiseDestinationError(
        submissionInfo.formId,
        destination.id,
        new Exception(DestinationSubmitter.handlebarsHttpApiFailOnErrorMessage(response))
      )

  private def createSuccessResponse(
    d: Destination.HandlebarsHttpApi,
    response: HttpResponse
  ): M[HandlebarsDestinationResponse] =
    monadError.pure(HandlebarsDestinationResponse(d, response))

  private def replaceStringsWithLengths(json: JsValue): JsValue = json match {
    case JsObject(fields) =>
      JsObject(fields.map { case (key, value) =>
        value match {
          case JsString(str) => (key, JsString(str.length.toString))
          case other         => (key, replaceStringsWithLengths(other))
        }
      })
    case JsArray(values) => JsArray(values.map(replaceStringsWithLengths))
    case _               => json
  }
}

object DestinationSubmitter {
  def handlebarsHttpApiFailOnErrorMessage(response: HttpResponse): String =
    s"Returned status code ${response.status} and has 'failOnError' set to true. Failing."
}
