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
import cats.syntax.EitherOps
import cats.syntax.eq._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.syntax.option._
import play.api.Logger
import play.api.libs.json.{ JsNull, JsValue, Json }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ DestinationTestResult, _ }
import uk.gov.hmrc.gform.submission.handlebars.{ HandlebarsDestinationResponse, HandlebarsHttpApiSubmitter, HandlebarsTemplateProcessor, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.gform.wshttp.HttpResponseSyntax

import scala.annotation.tailrec

trait DestinationSubmitter[M[_]] {
  def submitIfIncludeIf(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    model: HandlebarsTemplateProcessorModel)(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]]

  def submitToDms(submissionInfo: DestinationSubmissionInfo, submission: Destinations.DmsSubmission)(
    implicit hc: HeaderCarrier): M[Unit]
}

class RealDestinationSubmitter[M[_], R](
  dms: DmsSubmitter[M],
  handlebars: HandlebarsHttpApiSubmitter[M],
  handlebarsTemplateProcessor: HandlebarsTemplateProcessor = new HandlebarsTemplateProcessor)(
  implicit monadError: MonadError[M, String])
    extends DestinationSubmitter[M] {

  def submitIfIncludeIf(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    model: HandlebarsTemplateProcessorModel)(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    monadError.pure(destination.includeIf.forall(handlebarsTemplateProcessor(_, model) === true.toString)) flatMap {
      include =>
        if (include) submit(destination, submissionInfo, model)
        else Option.empty[HandlebarsDestinationResponse].pure
    }

  private def submit(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    model: HandlebarsTemplateProcessorModel)(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    destination match {
      case d: Destination.HmrcDms           => submitToDms(submissionInfo, d).map(_ => None)
      case d: Destination.HandlebarsHttpApi => submitToHandlebars(d, model)
    }

  def submitToDms(submissionInfo: DestinationSubmissionInfo, submission: Destinations.DmsSubmission)(
    implicit hc: HeaderCarrier): M[Unit] = dms(submissionInfo, submission)

  private def submitToDms(submissionInfo: DestinationSubmissionInfo, d: Destination.HmrcDms)(
    implicit hc: HeaderCarrier): M[Unit] =
    monadError.handleErrorWith(submitToDms(submissionInfo, d.toDeprecatedDmsSubmission)) { msg =>
      if (d.failOnErrorDefaulted)
        monadError.raiseError(msg)
      else {
        Logger.info(s"Destination ${d.id} failed but has 'failOnError' set to false. Ignoring.")
        monadError.pure(())
      }
    }

  private def submitToHandlebars(d: Destination.HandlebarsHttpApi, model: HandlebarsTemplateProcessorModel)(
    implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    handlebars(d, model)
      .flatMap[HandlebarsDestinationResponse] { response =>
        if (response.isSuccess)
          createSuccessResponse(d, response)
        else if (d.failOnErrorDefaulted)
          createFailureResponse(d, response)
        else {
          Logger.info(
            s"Destination ${d.id} returned status code ${response.status} but has 'failOnError' set to false. Ignoring.")
          createSuccessResponse(d, response)
        }
      }
      .map(Option(_))

  private def createFailureResponse(
    d: Destination.HandlebarsHttpApi,
    response: HttpResponse): M[HandlebarsDestinationResponse] =
    monadError.raiseError(RealDestinationSubmitter.handlebarsHttpApiFailOnErrorMessage(d, response))

  private def createSuccessResponse(
    d: Destination.HandlebarsHttpApi,
    response: HttpResponse): M[HandlebarsDestinationResponse] =
    monadError.pure(HandlebarsDestinationResponse(d, response))
}

object RealDestinationSubmitter {
  def handlebarsHttpApiFailOnErrorMessage(d: Destination.HandlebarsHttpApi, response: HttpResponse): String =
    s"Destination ${d.id} returned status code ${response.status} and has 'failOnError' set to true. Failing."
}

object SelfTestingDestinationSubmitter {
  type TestResult[A] = Either[String, A]

  implicit val testResultMonadError: MonadError[TestResult, String] = new MonadError[TestResult, String] {
    import cats.syntax.either._
    override def flatMap[A, B](fa: TestResult[A])(f: A => TestResult[B]): TestResult[B] = new EitherOps(fa).flatMap(f)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => TestResult[Either[A, B]]): TestResult[B] = f(a) match {
      case Left(s)         => s.asLeft
      case Right(Left(a2)) => tailRecM(a2)(f)
      case Right(Right(b)) => b.asRight
    }

    override def raiseError[A](e: String): TestResult[A] = e.asLeft

    override def handleErrorWith[A](fa: TestResult[A])(f: String => TestResult[A]): TestResult[A] = fa match {
      case Left(s)  => f(s)
      case Right(a) => a.asRight
    }

    override def pure[A](a: A): TestResult[A] = a.asRight
  }
}

class SelfTestingDestinationSubmitter[M[_]](
  handlebarsTemplateProcessor: HandlebarsTemplateProcessor = new HandlebarsTemplateProcessor,
  test: DestinationTest)(implicit monadError: MonadError[M, String])
    extends DestinationSubmitter[M] {

  override def submitToDms(submissionInfo: DestinationSubmissionInfo, submission: Destinations.DmsSubmission)(
    implicit hc: HeaderCarrier): M[Unit] = ().pure

  override def submitIfIncludeIf(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    model: HandlebarsTemplateProcessorModel)(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    test.expectedResults
      .find(_.destinationId === destination.id)
      .map(verifySpecifiedDestination(destination, model, _))
      .getOrElse(verifyUnspecifiedDestination(destination, model))

  private def verifyUnspecifiedDestination(
    destination: Destination,
    model: HandlebarsTemplateProcessorModel): M[Option[HandlebarsDestinationResponse]] =
    if (isIncludeIf(destination, model))
      fail(destination.id, "includeIf evaluated to true but no test destination information was provided.")
    else succeed(None)

  private def verifySpecifiedDestination(
    destination: Destination,
    model: HandlebarsTemplateProcessorModel,
    expected: DestinationTestResult): M[Option[HandlebarsDestinationResponse]] =
    (isIncludeIf(destination, model), expected.includeIf) match {
      case (false, false) => succeed(None)
      case (false, true) =>
        fail(destination.id, "includeIf specified in test was true, but the destination's includeIf evaluated to false")
      case (true, false) =>
        fail(destination.id, "includeIf specified in test was false, but the destination's includeIf evaluated to true")
      case (true, true) => verifyIncludedDestination(destination, model, expected)
    }

  private def verifyIncludedDestination(
    destination: Destination,
    model: HandlebarsTemplateProcessorModel,
    expected: DestinationTestResult): M[Option[HandlebarsDestinationResponse]] =
    destination match {
      case _: Destination.HmrcDms           => succeed(None)
      case d: Destination.HandlebarsHttpApi => verifyHandlebarsDestination(d, model, expected)
    }

  private def verifyHandlebarsDestination(
    destination: Destination.HandlebarsHttpApi,
    model: HandlebarsTemplateProcessorModel,
    expected: DestinationTestResult): M[Option[HandlebarsDestinationResponse]] = {
    val actualUri = handlebarsTemplateProcessor(destination.uri, model)
    if (actualUri =!= destination.uri)
      expected.uri match {
        case None =>
          fail(
            destination.id,
            s"No expected URI was specified for a templated destination URI. The template is '${destination.uri}'. After handlebars processing it was '$actualUri'"
          )
        case Some(expectedUri) =>
          if (handlebarsTemplateProcessor(destination.uri, model) =!= expectedUri)
            fail(
              destination.id,
              s"Expected URI '$expectedUri'. Got '${handlebarsTemplateProcessor(destination.uri, model)}'")
          else verifyHandlebarsDestinationPayload(destination, model, expected)
      } else verifyHandlebarsDestinationPayload(destination, model, expected)
  }

  private def verifyHandlebarsDestinationPayload(
    destination: Destination.HandlebarsHttpApi,
    model: HandlebarsTemplateProcessorModel,
    result: DestinationTestResult): M[Option[HandlebarsDestinationResponse]] =
    (destination.payload, result.payload) match {
      case (None, None) => succeed(destination.id, result)
      case (None, Some(_)) =>
        fail(
          destination.id,
          "Destination has no payload specified but the expectation in the test does specify a payload")
      case (Some(_), None) =>
        fail(destination.id, "Destination has a payload specified but the expectation in the test does not")
      case (Some(dp), Some(rp)) => verifyHandlebarsDestinationPayload(destination, model, result, dp, rp)
    }

  private def verifyHandlebarsDestinationPayload(
    destination: Destination.HandlebarsHttpApi,
    model: HandlebarsTemplateProcessorModel,
    result: DestinationTestResult,
    destinationPayloadTemplate: String,
    requiredPayload: JsValue): M[Option[HandlebarsDestinationResponse]] = {
    val processedDestinationPayload = handlebarsTemplateProcessor(destinationPayloadTemplate, model)
    parseTransformedPayload(destination.id, processedDestinationPayload).flatMap { json =>
      if (json != requiredPayload)
        fail(destination.id, "The generated payload does not match the expected payload.")
      else succeed(destination.id, result)
    }
  }

  private def parseTransformedPayload(id: DestinationId, s: String): M[JsValue] =
    try {
      monadError.pure(Json.parse(s))
    } catch {
      case ex: Exception =>
        fail(id, s"The generated payload is not valid JSON: ${ex.getMessage}")
    }

  private def isIncludeIf(destination: Destination, model: HandlebarsTemplateProcessorModel) =
    destination.includeIf.forall(handlebarsTemplateProcessor(_, model) === true.toString)

  private def fail[T](id: DestinationId, message: String): M[T] =
    monadError.raiseError(s"Test: ${test.name}, Destination: ${id.id}: $message")

  private def succeed(
    destinationId: DestinationId,
    result: DestinationTestResult): M[Option[HandlebarsDestinationResponse]] =
    result.response match {
      case None    => fail(destinationId, "No response specified. At least the response.code is needed.")
      case Some(r) => succeed(HandlebarsDestinationResponse(destinationId, r.code, r.json.getOrElse(JsNull)).some)
    }

  private def succeed(response: Option[HandlebarsDestinationResponse]): M[Option[HandlebarsDestinationResponse]] =
    response.pure
}
