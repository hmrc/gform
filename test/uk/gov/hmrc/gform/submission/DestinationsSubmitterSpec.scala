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
import com.fasterxml.jackson.databind.node.TextNode
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, Destinations, HandlebarsDestinationResponse, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, DestinationsGen }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import org.scalacheck.Gen
import play.api.libs.json.{ JsNull, JsNumber, JsObject, JsString }

class DestinationsSubmitterSpec extends Spec {
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  private def submissionInfoGen: Gen[DestinationSubmissionInfo] =
    DestinationSubmissionInfoGen.destinationSubmissionInfoGen.map {
      _.copy(form = form, formTemplate = formTemplate)
    }

  "Destinations.DmsSubmission" should "be sent to DestinationSubmitter" in {
    forAll(submissionInfoGen, DestinationsGen.deprecatedDmsSubmissionGen) { (submissionInfo, destination) =>
      val si = setSubmissionInfoDestinations(submissionInfo, destination)

      createSubmitter()
        .expectSubmitToDms(destination, si)
        .submitter
        .send(si)
    }
  }

  "Every Destination" should "be sent to the DestinationSubmitter when there is no includeIf" in {
    forAll(submissionInfoGen, DestinationGen.destinationGen) { (submissionInfo, generatedDestination) =>
      val destination = setIncludeIf(generatedDestination, None)
      val si = setSubmissionInfoDestinations(submissionInfo, destination)

      createSubmitter()
        .expectDestinationSubmitterSubmitIfIncludeIf(
          destination,
          si,
          DestinationsSubmitter.createHandlebarsTemplateProcessorModel(si),
          None)
        .submitter
        .send(si)
    }
  }

  "Subsequent Destination.HandlebarsHttpApi destinations" should "able to use the response codes and bodies from previous Destination.HandlebarsHttpApi destinations" in {
    forAll(
      submissionInfoGen,
      DestinationGen.handlebarsHttpApiGen,
      DestinationGen.handlebarsHttpApiGen,
      Gen.chooseNum(100, 599)
    ) { (submissionInfo, generatedHandlebarsHttpApi1, generatedHandlebarsHttpApi2, responseCode1) =>
      val handlebarsHttpApi1 = setIncludeIf(generatedHandlebarsHttpApi1, None)
      val handlebarsHttpApi2 = setIncludeIf(generatedHandlebarsHttpApi2, None)
      val si = setSubmissionInfoDestinations(submissionInfo, handlebarsHttpApi1, handlebarsHttpApi2)

      val responseJson1 = JsObject(
        Seq(
          "intField"    -> JsNumber(2),
          "stringField" -> JsString("stringNodeValue")
        ))

      val response1 = HttpResponse(responseCode1, Option(responseJson1))

      val initialModel = DestinationsSubmitter.createHandlebarsTemplateProcessorModel(si)
      val response1Model = HandlebarsDestinationResponse(handlebarsHttpApi1, response1)
      val expectedModel2 = initialModel + HandlebarsTemplateProcessorModel(response1Model)

      createSubmitter()
        .expectDestinationSubmitterSubmitIfIncludeIf(handlebarsHttpApi1, si, initialModel, Option(response1Model))
        .expectDestinationSubmitterSubmitIfIncludeIf(handlebarsHttpApi2, si, expectedModel2, None)
        .submitter
        .send(si)
    }
  }

  "createResponseModel" should "build the appropriate JSON" in {
    forAll(DestinationGen.handlebarsHttpApiGen, Gen.chooseNum(100, 599)) { (destination, responseCode) =>
      val responseBody = JsObject(
        Seq(
          "intField"    -> JsNumber(2),
          "stringField" -> JsString("stringNodeValue")
        ))

      val responseModel =
        HandlebarsTemplateProcessorModel(
          HandlebarsDestinationResponse(destination, HttpResponse(responseCode, Option(responseBody))))

      responseModel.model.toString shouldBe
        JsObject(
          Seq(
            s"${destination.id.id}" -> JsObject(
              Seq(
                "status" -> JsNumber(responseCode),
                "json"   -> responseBody
              ))
          )).toString
    }
  }

  it should "contain a JsNull when the response body is empty or cannot be parsed" in {
    forAll(DestinationGen.handlebarsHttpApiGen, Gen.chooseNum(100, 599)) { (destination, responseCode) =>
      val result = HandlebarsDestinationResponse(destination, HttpResponse(responseCode, responseString = Option("")))

      val responseModel = HandlebarsTemplateProcessorModel(result)

      responseModel.model.toString shouldBe
        JsObject(
          Seq(
            s"${destination.id.id}" -> JsObject(
              Seq(
                "status" -> JsNumber(responseCode),
                "json"   -> JsNull
              ))
          )).toString
    }
  }

  private def setIncludeIf[T <: Destination](destination: T, includeIf: Option[String]): T =
    destination match {
      case d: Destination.HmrcDms           => d.copy(includeIf = includeIf).asInstanceOf[T]
      case d: Destination.HandlebarsHttpApi => d.copy(includeIf = includeIf).asInstanceOf[T]
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
    destinationSubmitter: DestinationSubmitter[F]) {

    def expectDestinationSubmitterSubmitIfIncludeIf(
      destination: Destination,
      submissionInfo: DestinationSubmissionInfo,
      model: HandlebarsTemplateProcessorModel,
      response: Option[HandlebarsDestinationResponse]): SubmitterParts[F] = {
      (destinationSubmitter
        .submitIfIncludeIf(_: Destination, _: DestinationSubmissionInfo, _: HandlebarsTemplateProcessorModel)(
          _: HeaderCarrier))
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
  }

  private def createSubmitter[M[_]: Monad](): SubmitterParts[M] = {
    val destinationSubmitter: DestinationSubmitter[M] = mock[DestinationSubmitter[M]]
    val submitter = new DestinationsSubmitter[M](destinationSubmitter)

    SubmitterParts(submitter, destinationSubmitter)
  }
}
