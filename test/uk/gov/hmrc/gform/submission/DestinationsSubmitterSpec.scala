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
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, Destinations }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, DestinationsGen }
import uk.gov.hmrc.gform.submission.handlebars.{ HandlebarsHttpApiSubmitter, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import org.scalacheck.Gen
import play.api.libs.json.{ JsNumber, JsObject, JsString }

class DestinationsSubmitterSpec extends Spec {
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  "Destinations.DmsSubmission" should "be sent to the DmsSubmitter" in {
    forAll(DestinationSubmissionInfoGen.destinationSubmissionInfoGen, DestinationsGen.deprecatedDmsSubmissionGen) {
      (submissionInfo, dmsSubmission) =>
        val si = submissionInfo.copy(formTemplate = submissionInfo.formTemplate.copy(destinations = dmsSubmission))
        val sp = createSubmitter()
        sp.expectDmsSubmission(si, dmsSubmission)
        sp.submitter.send(si)
    }
  }

  "A Destination.HmrcDms" should "be sent to the DmsSubmitter" in {
    forAll(DestinationSubmissionInfoGen.destinationSubmissionInfoGen, DestinationGen.hmrcDmsGen) {
      (submissionInfo, hmrcDms) =>
        val si = submissionInfo.copy(
          formTemplate =
            submissionInfo.formTemplate.copy(destinations = Destinations.DestinationList(NonEmptyList.of(hmrcDms))))

        val sp = createSubmitter()
        sp.expectDmsSubmission(si, hmrcDms.toDeprecatedDmsSubmission)
        sp.submitter.send(si)
    }
  }

  "A Destination.HandlebarsHttpApi" should "be sent to the HandlebarsHttpApiSubmitter" in {
    forAll(DestinationSubmissionInfoGen.destinationSubmissionInfoGen, DestinationGen.handlebarsHttpApiGen) {
      (submissionInfo, handlebarsHttpApi) =>
        val si = submissionInfo.copy(
          formTemplate = submissionInfo.formTemplate.copy(
            destinations = Destinations.DestinationList(NonEmptyList.of(handlebarsHttpApi))))

        val sp = createSubmitter()
        sp.expectHandlebarsSubmission(handlebarsHttpApi, HandlebarsTemplateProcessorModel(si.form), HttpResponse(200))
        sp.submitter.send(si)
    }
  }

  "Subsequent Destination.HandlebarsHttpApi destinations should be able to use the response codes and bodies from previous Destination.HandlebarsHttpApi destinations" should "be sent to the HandlebarsHttpApiSubmitter" in {
    forAll(
      DestinationSubmissionInfoGen.destinationSubmissionInfoGen,
      DestinationGen.handlebarsHttpApiGen,
      DestinationGen.handlebarsHttpApiGen,
      Gen.chooseNum(100, 599)
    ) { (submissionInfo, handlebarsHttpApi1, handlebarsHttpApi2, responseCode1) =>
      val si = submissionInfo.copy(
        formTemplate = submissionInfo.formTemplate.copy(
          destinations = Destinations.DestinationList(NonEmptyList.of(handlebarsHttpApi1, handlebarsHttpApi2))))

      val responseJson1 = JsObject(
        Seq(
          "intField"    -> JsNumber(2),
          "stringField" -> JsString("stringNodeValue")
        ))

      val response1 = HttpResponse(responseCode1, Option(responseJson1))

      val initialModel = HandlebarsTemplateProcessorModel(si.form)
      val expectedModel2 = initialModel + DestinationsSubmitter.createResponseModel(handlebarsHttpApi1, response1)

      createSubmitter()
        .expectHandlebarsSubmission(handlebarsHttpApi1, initialModel, response1)
        .expectHandlebarsSubmission(handlebarsHttpApi2, expectedModel2, HttpResponse(200))
        .submitter
        .send(si)
    }
  }

  case class SubmitterParts[F[_]](
    submitter: DestinationsSubmitter[F],
    dmsSubmitter: DmsSubmitter[F],
    handlebarsSubmitter: HandlebarsHttpApiSubmitter[F]) {
    def expectDmsSubmission(si: DestinationSubmissionInfo, dms: Destinations.DmsSubmission)(
      implicit F: Applicative[F]): SubmitterParts[F] = {
      (dmsSubmitter
        .apply(_: DestinationSubmissionInfo, _: Destinations.DmsSubmission))
        .expects(si, dms)
        .returning(F.pure(()))
      this
    }

    def expectHandlebarsSubmission(
      handlebarsHttpApi: Destination.HandlebarsHttpApi,
      model: HandlebarsTemplateProcessorModel,
      response: F[HttpResponse]): SubmitterParts[F] = {
      (handlebarsSubmitter
        .apply(_: Destination.HandlebarsHttpApi, _: HandlebarsTemplateProcessorModel)(_: HeaderCarrier))
        .expects(handlebarsHttpApi, model, hc)
        .returning(response)
      this
    }
  }

  private def createSubmitter[F[_]: Monad](): SubmitterParts[F] = {
    val dmsSubmitter = mock[DmsSubmitter[F]]
    val handlebarsSubmitter = mock[HandlebarsHttpApiSubmitter[F]]
    val submitter = new DestinationsSubmitter[F](dmsSubmitter, handlebarsSubmitter)

    SubmitterParts(submitter, dmsSubmitter, handlebarsSubmitter)
  }
}
