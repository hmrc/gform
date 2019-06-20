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
import cats.syntax.option._
import play.api.Logger
import play.api.libs.json.{ JsNull, JsValue, Json }
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.sharedmodel.form.{ DestinationSubmissionInfo, FormId, FormStatus }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ DestinationTestResult, _ }
import uk.gov.hmrc.gform.submission.handlebars.{ HandlebarsHttpApiSubmitter, HandlebarsTemplateProcessor, RealHandlebarsTemplateProcessor }
import uk.gov.hmrc.gform.submission.ofsted.OfstedSubmitter
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.gform.wshttp.HttpResponseSyntax

trait DestinationSubmitter[M[_]] {
  def submitIfIncludeIf(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    model: HandlebarsTemplateProcessorModel,
    submitter: DestinationsSubmitterAlgebra[M],
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]]

  def submitToDms(submissionInfo: DestinationSubmissionInfo, submission: Destinations.DmsSubmission)(
    implicit hc: HeaderCarrier): M[Unit]
}

class RealDestinationSubmitter[M[_], R](
  dms: DmsSubmitter[M],
  handlebars: HandlebarsHttpApiSubmitter[M],
  ofsted: OfstedSubmitter[M],
  destinationAuditer: DestinationAuditAlgebra[M],
  formAlgebra: FormAlgebra[M],
  handlebarsTemplateProcessor: HandlebarsTemplateProcessor = new RealHandlebarsTemplateProcessor)(
  implicit monadError: MonadError[M, String])
    extends DestinationSubmitter[M] {

  def submitIfIncludeIf(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    model: HandlebarsTemplateProcessorModel,
    submitter: DestinationsSubmitterAlgebra[M],
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    monadError.pure(DestinationSubmitter.isIncludeIf(destination, model, handlebarsTemplateProcessor)) flatMap {
      include =>
        if (include)
          for {
            _      <- logInfoInMonad(s"Destination ${destination.id.id} is included")
            result <- submit(destination, submissionInfo, model, submitter, formTemplate)
            _      <- destinationAuditer(destination.id, result.map(_.status), submissionInfo.formId)
          } yield result
        else
          for {
            _      <- logInfoInMonad(s"Destination ${destination.id.id} is not included")
            result <- Option.empty[HandlebarsDestinationResponse].pure
          } yield result

    }

  private def logInfoInMonad(msg: String): M[Unit] =
    monadError.pure {
      Logger.info(msg)
    }

  private def submit(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    model: HandlebarsTemplateProcessorModel,
    submitter: DestinationsSubmitterAlgebra[M],
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    destination match {
      case d: Destination.HmrcDms           => submitToDms(submissionInfo, d).map(_ => None)
      case d: Destination.HandlebarsHttpApi => submitToHandlebars(d, model, submissionInfo.formId)
      case d: Destination.Composite =>
        submitter.submitToList(DestinationList(d.destinations), submissionInfo, model, formTemplate)
      case d: Destination.StateTransition => transitionState(d, submissionInfo.formId).map(_ => None)
      case d: Destination.ReviewingOfsted => submitForReview(d, submissionInfo).map(_ => None)
      case d: Destination.ReviewRejection => submitToReviewRejection(d, submissionInfo.formId).map(_ => None)
      case d: Destination.ReviewApproval =>
        submitToReviewApproval(d, submissionInfo.formId, submitter, formTemplate).map(_ => None)
    }

  def transitionState(d: Destination.StateTransition, formId: FormId)(implicit hc: HeaderCarrier): M[Unit] =
    formAlgebra
      .updateFormStatus(formId, d.requiredState)
      .flatMap { stateAchieved =>
        if (stateAchieved === d.requiredState || !d.failOnError) monadError.pure(())
        else raiseError(d.id, RealDestinationSubmitter.stateTransitionFailOnErrorMessage(d, stateAchieved))
      }

  def submitToReviewApproval(
    destination: Destination.ReviewApproval,
    reviewFormId: FormId,
    submitter: DestinationsSubmitterAlgebra[M],
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): M[Unit] =
    ofsted
      .approve(
        reviewFormId,
        destination.correlationFieldId,
        submitter,
        formTemplate
      )
      .void

  def submitToReviewRejection(destination: Destination.ReviewRejection, reviewFormId: FormId)(
    implicit hc: HeaderCarrier): M[Unit] =
    ofsted
      .reject(
        reviewFormId,
        destination.correlationFieldId,
        destination.reviewFormCommentFieldId
      )
      .void

  def submitForReview(destination: Destination.ReviewingOfsted, submissionInfo: DestinationSubmissionInfo)(
    implicit hc: HeaderCarrier): M[Unit] =
    ofsted
      .submitForReview(
        submissionInfo,
        destination.userId,
        destination.reviewFormTemplateId,
        destination.correlationFieldId
      )
      .void

  def submitToDms(submissionInfo: DestinationSubmissionInfo, submission: Destinations.DmsSubmission)(
    implicit hc: HeaderCarrier): M[Unit] = dms(submissionInfo, submission)

  private def submitToDms(submissionInfo: DestinationSubmissionInfo, d: Destination.HmrcDms)(
    implicit hc: HeaderCarrier): M[Unit] =
    monadError.handleErrorWith(submitToDms(submissionInfo, d.toDeprecatedDmsSubmission)) { msg =>
      if (d.failOnError)
        monadError.raiseError(s"Destination ${d.id.id} : $msg")
      else {
        Logger.info(s"Destination ${d.id} failed but has 'failOnError' set to false. Ignoring.")
        monadError.pure(())
      }
    }

  private def submitToHandlebars(
    d: Destination.HandlebarsHttpApi,
    model: HandlebarsTemplateProcessorModel,
    formId: FormId)(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    handlebars(d, model)
      .flatMap[HandlebarsDestinationResponse] { response =>
        if (response.isSuccess)
          createSuccessResponse(d, response)
        else if (d.failOnError)
          createFailureResponse(d, response, formId)
        else {
          Logger.info(
            s"Destination ${d.id} returned status code ${response.status} but has 'failOnError' set to false. Ignoring.")
          createSuccessResponse(d, response)
        }
      }
      .map(Option(_))

  private def createFailureResponse(d: Destination.HandlebarsHttpApi, response: HttpResponse, formId: FormId)(
    implicit hc: HeaderCarrier): M[HandlebarsDestinationResponse] =
    destinationAuditer(d.id, Some(response.status), formId).flatMap { _ =>
      monadError.raiseError(RealDestinationSubmitter.handlebarsHttpApiFailOnErrorMessage(d, response))
    }

  private def createSuccessResponse(
    d: Destination.HandlebarsHttpApi,
    response: HttpResponse): M[HandlebarsDestinationResponse] =
    monadError.pure(HandlebarsDestinationResponse(d, response))

  protected def raiseError[T](destinationId: DestinationId, msg: String) =
    monadError.raiseError[T](s"Destination ${destinationId.id} : $msg")
}

object DestinationSubmitter {
  def isIncludeIf(
    destination: Destination,
    model: HandlebarsTemplateProcessorModel,
    handlebarsTemplateProcessor: HandlebarsTemplateProcessor): Boolean =
    handlebarsTemplateProcessor(destination.includeIf, model) === true.toString
}

object RealDestinationSubmitter {
  def handlebarsHttpApiFailOnErrorMessage(d: Destination.HandlebarsHttpApi, response: HttpResponse): String =
    s"Destination ${d.id} returned status code ${response.status} and has 'failOnError' set to true. Failing."

  def stateTransitionFailOnErrorMessage(d: Destination.StateTransition, currentState: FormStatus): String =
    s"Destination ${d.id} cannot achieve transition from $currentState to ${d.requiredState}"
}

object SelfTestingDestinationSubmitter {
  type TestResult[A] = Either[String, A]
}

class SelfTestingDestinationSubmitter[M[_]](
  handlebarsTemplateProcessor: HandlebarsTemplateProcessor = new RealHandlebarsTemplateProcessor,
  test: DestinationTest)(implicit monadError: MonadError[M, String])
    extends DestinationSubmitter[M] {
  type ReturnType = M[Option[HandlebarsDestinationResponse]]

  override def submitToDms(submissionInfo: DestinationSubmissionInfo, submission: Destinations.DmsSubmission)(
    implicit hc: HeaderCarrier): M[Unit] = ().pure

  override def submitIfIncludeIf(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    model: HandlebarsTemplateProcessorModel,
    submitter: DestinationsSubmitterAlgebra[M],
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    test.expectedResults
      .find(_.destinationId === destination.id)
      .map(verifySpecifiedDestination(destination, model, _, submitter, formTemplate))
      .getOrElse(verifyUnspecifiedDestination(destination, model))

  private def verifyUnspecifiedDestination(
    destination: Destination,
    model: HandlebarsTemplateProcessorModel): ReturnType =
    if (DestinationSubmitter.isIncludeIf(destination, model, handlebarsTemplateProcessor))
      includeIEvaluatedToTrueButNoTestDestinationInformationWasProvided(destination)
    else succeed(None)

  private[submission] def includeIEvaluatedToTrueButNoTestDestinationInformationWasProvided(
    destination: Destination): ReturnType =
    fail(destination.id, "includeIf evaluated to true but no test destination information was provided.")

  private def verifySpecifiedDestination(
    destination: Destination,
    model: HandlebarsTemplateProcessorModel,
    expected: DestinationTestResult,
    destinationsSubmitter: DestinationsSubmitterAlgebra[M],
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): ReturnType =
    (DestinationSubmitter.isIncludeIf(destination, model, handlebarsTemplateProcessor), expected.includeIf) match {
      case (false, false)     => succeed(None)
      case (true, true)       => verifyIncludedDestination(destination, model, expected, destinationsSubmitter, formTemplate)
      case (actual, expected) => inconsistentIncludeIfs(destination, actual, expected)
    }

  private[submission] def inconsistentIncludeIfs(
    destination: Destination,
    actual: Boolean,
    expected: Boolean): ReturnType =
    fail(
      destination.id,
      s"includeIf specified in test was $expected, but the destination's includeIf evaluated to $actual")

  private def verifyIncludedDestination(
    destination: Destination,
    model: HandlebarsTemplateProcessorModel,
    expected: DestinationTestResult,
    destinationsSubmitter: DestinationsSubmitterAlgebra[M],
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    destination match {
      case _: Destination.HmrcDms           => succeed(None)
      case d: Destination.HandlebarsHttpApi => verifyHandlebarsDestination(d, model, expected)
      case d: Destination.Composite =>
        destinationsSubmitter.submitToList(DestinationList(d.destinations), null, model, formTemplate)
      case _: Destination.StateTransition => succeed(None)
      case _: Destination.ReviewingOfsted => succeed(None)
      case _: Destination.ReviewRejection => succeed(None)
      case _: Destination.ReviewApproval  => succeed(None)
    }

  private def verifyHandlebarsDestination(
    destination: Destination.HandlebarsHttpApi,
    model: HandlebarsTemplateProcessorModel,
    expected: DestinationTestResult): M[Option[HandlebarsDestinationResponse]] =
    if (isTemplated(destination.uri))
      expected.uri match {
        case None => noExpectedUriSpecified(destination)
        case Some(expectedUri) =>
          val actualUri = handlebarsTemplateProcessor(destination.uri, model)
          if (actualUri =!= expectedUri) mismatchedUri(destination, expectedUri, actualUri)
          else verifyHandlebarsDestinationPayload(destination, model, expected)
      } else verifyHandlebarsDestinationPayload(destination, model, expected)

  private def isTemplated(s: String) = s.contains("{{")

  private[submission] def noExpectedUriSpecified(destination: Destination.HandlebarsHttpApi) =
    fail[Option[HandlebarsDestinationResponse]](
      destination.id,
      s"No expected URI was specified for a templated destination URI. The template is '${destination.uri}'."
    )

  private[submission] def mismatchedUri(
    destination: Destination.HandlebarsHttpApi,
    expectedUri: String,
    actualUri: String) =
    fail[Option[HandlebarsDestinationResponse]](destination.id, s"Expected URI '$expectedUri'. Got '$actualUri'")

  private def verifyHandlebarsDestinationPayload(
    destination: Destination.HandlebarsHttpApi,
    model: HandlebarsTemplateProcessorModel,
    result: DestinationTestResult): M[Option[HandlebarsDestinationResponse]] =
    (destination.payload, result.payload) match {
      case (None, None)         => succeed(destination.id, result)
      case (None, Some(_))      => destinationHasNoPayloadButTestDoes(destination)
      case (Some(_), None)      => testHasNoPayloadButDestinationDoes(destination)
      case (Some(dp), Some(rp)) => verifyHandlebarsDestinationPayload(destination, model, result, dp, rp)
    }

  private[submission] def destinationHasNoPayloadButTestDoes(destination: Destination) =
    fail[Option[HandlebarsDestinationResponse]](
      destination.id,
      "Destination has no payload specified but the expectation in the test does specify a payload")

  private[submission] def testHasNoPayloadButDestinationDoes(destination: Destination) =
    fail[Option[HandlebarsDestinationResponse]](
      destination.id,
      "Destination has a payload specified but the expectation in the test does not")

  private def verifyHandlebarsDestinationPayload(
    destination: Destination.HandlebarsHttpApi,
    model: HandlebarsTemplateProcessorModel,
    result: DestinationTestResult,
    destinationPayloadTemplate: String,
    requiredPayload: JsValue): M[Option[HandlebarsDestinationResponse]] = {
    val processedDestinationPayload = handlebarsTemplateProcessor(destinationPayloadTemplate, model)
    parseTransformedPayload(destination.id, processedDestinationPayload).flatMap { json =>
      if (json != requiredPayload)
        generatedPayloadDoesNotMatchExpected(destination)
      else succeed(destination.id, result)
    }
  }

  private[submission] def generatedPayloadDoesNotMatchExpected(destination: Destination) =
    fail[Option[HandlebarsDestinationResponse]](
      destination.id,
      "The generated payload does not match the expected payload.")

  private[submission] def parseTransformedPayload(id: DestinationId, s: String): M[JsValue] =
    try { monadError.pure(Json.parse(s)) } catch {
      case ex: Exception =>
        theGeneratedPayloadIsNotValidJson(id, ex.getMessage)
    }

  private[submission] def theGeneratedPayloadIsNotValidJson(id: DestinationId, message: String) =
    fail[JsValue](id, s"The generated payload is not valid JSON: $message")

  private def fail[T](id: DestinationId, message: String): M[T] =
    monadError.raiseError(s"Test: ${test.name}, Destination: ${id.id}: $message")

  private def succeed(
    destinationId: DestinationId,
    result: DestinationTestResult): M[Option[HandlebarsDestinationResponse]] =
    result.response match {
      case None    => noResponseSpecified(destinationId)
      case Some(r) => succeed(HandlebarsDestinationResponse(destinationId, r.status, r.json.getOrElse(JsNull)).some)
    }

  private[submission] def noResponseSpecified(destinationId: DestinationId) =
    fail[Option[HandlebarsDestinationResponse]](
      destinationId,
      "No response specified. At least the response.status is needed.")

  private def succeed(response: Option[HandlebarsDestinationResponse]): M[Option[HandlebarsDestinationResponse]] =
    response.pure
}
