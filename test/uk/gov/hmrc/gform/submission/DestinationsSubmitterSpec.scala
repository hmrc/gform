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

import cats.data.NonEmptyList
import cats.{ Applicative, Monad }
import cats.syntax.applicative._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, Destinations }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, DestinationsGen, PrimitiveGen }
import uk.gov.hmrc.gform.submission.handlebars.{ HandlebarsTemplateProcessor, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import org.scalacheck.Gen
import play.api.libs.json.{ JsNumber, JsObject, JsString }

class DestinationsSubmitterSpec extends Spec {
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  "Destinations.DmsSubmission" should "be sent to DestinationSubmitter" in {
    forAll(DestinationSubmissionInfoGen.destinationSubmissionInfoGen, DestinationsGen.deprecatedDmsSubmissionGen) {
      (submissionInfo, destination) =>
        val si = setSubmissionInfoDestinations(submissionInfo, destination)

        createSubmitter()
          .expectSubmitToDms(destination, si)
          .submitter
          .send(si)
    }
  }

  "Every Destination" should "be sent to the DestinationSubmitter when there is no includeIf" in {
    forAll(DestinationSubmissionInfoGen.destinationSubmissionInfoGen, DestinationGen.destinationGen) {
      (submissionInfo, destination) =>
        val si = setSubmissionInfoDestinations(submissionInfo, destination)

        createSubmitter()
          .expectDestinationSubmitterApply(
            destination,
            si,
            DestinationsSubmitter.createHandlebarsTemplateProcessorModel(si),
            None)
          .submitter
          .send(si)
    }
  }

  it should "be sent to the DestinationSubmitter when there is an includeIf that evaluates to true" in {
    forAll(
      DestinationSubmissionInfoGen.destinationSubmissionInfoGen,
      DestinationGen.destinationGen,
      PrimitiveGen.nonEmptyAlphaNumStrGen) { (submissionInfo, destinationWithoutIncludeIf, includeIf) =>
      val destination = setIncludeIf(destinationWithoutIncludeIf, includeIf)
      val si = setSubmissionInfoDestinations(submissionInfo, destination)
      val initialModel = HandlebarsTemplateProcessorModel(si.form)

      createSubmitter()
        .expectIncludeIfTemplateSubstitution(includeIf, initialModel, "true")
        .expectDestinationSubmitterApply(
          destination,
          si,
          DestinationsSubmitter.createHandlebarsTemplateProcessorModel(si),
          None)
        .submitter
        .send(si)
    }
  }

  it should "NOT be sent to the DestinationSubmitter when there is an includeIf that does not evaluate to true" in {
    forAll(
      DestinationSubmissionInfoGen.destinationSubmissionInfoGen,
      DestinationGen.destinationGen,
      PrimitiveGen.nonEmptyAlphaNumStrGen) { (submissionInfo, destinationWithoutIncludeIf, includeIf) =>
      val destination = setIncludeIf(destinationWithoutIncludeIf, includeIf)
      val si = setSubmissionInfoDestinations(submissionInfo, destination)
      val initialModel = HandlebarsTemplateProcessorModel(si.form)

      createSubmitter()
        .expectIncludeIfTemplateSubstitution(includeIf, initialModel, "false")
        .submitter
        .send(si)
    }
  }

  "Subsequent Destination.HandlebarsHttpApi destinations should be able to use the response codes and bodies from previous Destination.HandlebarsHttpApi destinations" should "be sent to the HandlebarsHttpApiSubmitter" in {
    forAll(
      DestinationSubmissionInfoGen.destinationSubmissionInfoGen,
      DestinationGen.handlebarsHttpApiGen,
      DestinationGen.handlebarsHttpApiGen,
      Gen.chooseNum(100, 599)
    ) { (submissionInfo, handlebarsHttpApi1, handlebarsHttpApi2, responseCode1) =>
      val si = setSubmissionInfoDestinations(submissionInfo, handlebarsHttpApi1, handlebarsHttpApi2)

      val responseJson1 = JsObject(
        Seq(
          "intField"    -> JsNumber(2),
          "stringField" -> JsString("stringNodeValue")
        ))

      val response1 = HttpResponse(responseCode1, Option(responseJson1))

      val initialModel = HandlebarsTemplateProcessorModel(si.form)
      val response1Model = DestinationsSubmitter.createResponseModel(handlebarsHttpApi1, response1)
      val expectedModel2 = initialModel + response1Model

      createSubmitter()
        .expectDestinationSubmitterApply(handlebarsHttpApi1, si, initialModel, Option(response1Model))
        .expectDestinationSubmitterApply(handlebarsHttpApi2, si, expectedModel2, None)
        .submitter
        .send(si)
    }
  }

  private def setIncludeIf(destination: Destination, includeIf: String): Destination =
    destination match {
      case d: Destination.HmrcDms           => d.copy(includeIf = Option(includeIf))
      case d: Destination.HandlebarsHttpApi => d.copy(includeIf = Option(includeIf))
    }

  private def setSubmissionInfoDestinations(
    si: DestinationSubmissionInfo,
    destination1: Destination,
    moreDestinations: Destination*): DestinationSubmissionInfo =
    setSubmissionInfoDestinations(si, Destinations.DestinationList(NonEmptyList.of(destination1, moreDestinations: _*)))

  private def setSubmissionInfoDestinations(
    si: DestinationSubmissionInfo,
    destinations: Destinations): DestinationSubmissionInfo =
    si.copy(formTemplate = si.formTemplate.copy(destinations = destinations))

  case class SubmitterParts[F[_]: Applicative](
    submitter: DestinationsSubmitter[F],
    destinationSubmitter: DestinationSubmitter[F],
    handlebarsTemplateProcessor: HandlebarsTemplateProcessor) {

    def expectDestinationSubmitterApply(
      destination: Destination,
      submissionInfo: DestinationSubmissionInfo,
      model: HandlebarsTemplateProcessorModel,
      response: Option[HandlebarsTemplateProcessorModel]): SubmitterParts[F] = {
      (destinationSubmitter
        .apply(_: Destination, _: DestinationSubmissionInfo, _: HandlebarsTemplateProcessorModel)(_: HeaderCarrier))
        .expects(destination, submissionInfo, model, hc)
        .returning(response.pure)
      this
    }

    def expectSubmitToDms(
      destination: Destinations.DmsSubmission,
      submissionInfo: DestinationSubmissionInfo): SubmitterParts[F] = {
      (destinationSubmitter
        .submitToDms(_: DestinationSubmissionInfo, _: Destinations.DmsSubmission)(_: HeaderCarrier))
        .expects(submissionInfo, destination, hc)
        .returning(().pure)
      this
    }

    def expectIncludeIfTemplateSubstitution(
      template: String,
      model: HandlebarsTemplateProcessorModel,
      result: String): SubmitterParts[F] = {
      (handlebarsTemplateProcessor
        .apply(_: String, _: HandlebarsTemplateProcessorModel))
        .expects(template, model)
        .returning(result)

      this
    }
  }

  private def createSubmitter[M[_]: Monad](): SubmitterParts[M] = {
    val destinationSubmitter: DestinationSubmitter[M] = mock[DestinationSubmitter[M]]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]
    val submitter = new DestinationsSubmitter[M](destinationSubmitter, handlebarsTemplateProcessor)

    SubmitterParts(submitter, destinationSubmitter, handlebarsTemplateProcessor)
  }
}
