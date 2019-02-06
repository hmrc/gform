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

package uk.gov.hmrc.gform.submission.handlebars

import cats.Id
import org.scalacheck.Gen
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ HttpMethod, Profile }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, PrimitiveGen }
import uk.gov.hmrc.gform.wshttp.JsonHttpClient
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

class HandlebarsHttpApiSubmitterSpec extends Spec {
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  "selectHttpClient" should "select the appropriate des or mdg http client, based on the profile" in {
    forAll(submitterPartsGen[Id]) { sp =>
      import sp._

      RealHandlebarsHttpApiSubmitter.selectHttpClient[Id](Profile.DES, des, mdg) shouldBe des
      RealHandlebarsHttpApiSubmitter.selectHttpClient[Id](Profile.MdgIntegrationFramework, des, mdg) shouldBe mdg
    }
  }

  "A GET destination" should "make a GET request, applying the template to the URI" in {
    forAll(
      DestinationGen.handlebarsHttpApiGen.map(_.copy(method = HttpMethod.GET)),
      submitterPartsGen[Id],
      PrimitiveGen.urlContextPathGen
    ) { (destination, sp, expectedUri) =>
      val processorModel = HandlebarsTemplateProcessorModel("")
      val expectedResponse = mock[HttpResponse]

      sp.expectTemplateProcessorApplication(destination.uri, processorModel, expectedUri)
        .expectHttpGet(destination.profile, expectedUri, expectedResponse)

      sp.submitter.apply(destination, processorModel) shouldBe expectedResponse
    }
  }

  "A POST destination" should "make a POST request when there is a payload" in {
    forAll(
      DestinationGen.handlebarsHttpApiGen.map(_.copy(method = HttpMethod.POST)),
      Gen.alphaNumStr,
      submitterPartsGen[Id],
      PrimitiveGen.urlContextPathGen,
      Gen.alphaNumStr
    ) { (d, payload, sp, expectedUri, expectedBody) =>
      val destination = d.copy(payload = Option(payload))
      val processorModel = HandlebarsTemplateProcessorModel("")
      val expectedResponse = mock[HttpResponse]

      sp.expectTemplateProcessorApplication(destination.uri, processorModel, expectedUri)
        .expectTemplateProcessorApplication(payload, processorModel, expectedBody)
        .expectHttpPostJson(destination.profile, expectedBody, expectedUri, expectedResponse)

      sp.submitter.apply(destination, processorModel) shouldBe expectedResponse
    }
  }

  it should "make a POST request when there is no payload" in {
    forAll(
      DestinationGen.handlebarsHttpApiGen.map(_.copy(method = HttpMethod.POST)),
      submitterPartsGen[Id],
      PrimitiveGen.urlContextPathGen) { (d, sp, expectedUri) =>
      val destination = d.copy(payload = None)
      val processorModel = HandlebarsTemplateProcessorModel("")
      val expectedResponse = mock[HttpResponse]

      sp.expectTemplateProcessorApplication(destination.uri, processorModel, expectedUri)
        .expectHttpPostJson(destination.profile, "", expectedUri, expectedResponse)

      sp.submitter.apply(destination, processorModel) shouldBe expectedResponse
    }
  }

  case class SubmitterParts[F[_]](
    submitter: HandlebarsHttpApiSubmitter[F],
    des: JsonHttpClient[F],
    mdg: JsonHttpClient[F],
    templateProcessor: HandlebarsTemplateProcessor) {

    def expectTemplateProcessorApplication(
      in: String,
      model: HandlebarsTemplateProcessorModel,
      out: String): SubmitterParts[F] = {
      (templateProcessor
        .apply(_: String, _: HandlebarsTemplateProcessorModel))
        .expects(in, model)
        .returning(out)

      this
    }

    def expectHttpGet(profile: Profile, expectedUri: String, expectedResponse: F[HttpResponse]): SubmitterParts[F] = {
      (RealHandlebarsHttpApiSubmitter
        .selectHttpClient(profile, des, mdg)
        .get(_: String)(_: HeaderCarrier))
        .expects(expectedUri, hc)
        .returning(expectedResponse)

      this
    }

    def expectHttpPostJson(
      profile: Profile,
      expectedBody: String,
      expectedUri: String,
      expectedResponse: F[HttpResponse]): SubmitterParts[F] = {
      (RealHandlebarsHttpApiSubmitter
        .selectHttpClient(profile, des, mdg)
        .postJsonString(_: String, _: String)(_: HeaderCarrier))
        .expects(expectedUri, expectedBody, hc)
        .returning(expectedResponse)

      this
    }
  }

  private def submitterPartsGen[F[_]]: Gen[SubmitterParts[F]] = {
    val desHttpClient = mock[JsonHttpClient[F]]
    val mdgIntegrationFrameworkHttpClient = mock[JsonHttpClient[F]]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]

    val submitter =
      new RealHandlebarsHttpApiSubmitter(desHttpClient, mdgIntegrationFrameworkHttpClient, handlebarsTemplateProcessor)

    SubmitterParts(submitter, desHttpClient, mdgIntegrationFrameworkHttpClient, handlebarsTemplateProcessor)
  }
}
