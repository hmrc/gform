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
import uk.gov.hmrc.gform.wshttp.HttpClient
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

  "A GET destination" should "make a GET request" in {
    forAll(DestinationGen.handlebarsHttpApiGen.map(_.copy(method = HttpMethod.GET)), submitterPartsGen[Id]) {
      (destination, sp) =>
        import sp._

        val expectedResponse = mock[HttpResponse]
        (RealHandlebarsHttpApiSubmitter
          .selectHttpClient[Id](destination.profile, des, mdg)
          .get(_: String)(_: HeaderCarrier))
          .expects(destination.uri, hc)
          .returning(expectedResponse)

        submitter.apply(destination, HandlebarsTemplateProcessorModel("")) shouldBe expectedResponse
    }
  }

  "A POST destination" should "make a POST request when there is a payload" in {
    forAll(
      DestinationGen.handlebarsHttpApiGen.map(_.copy(method = HttpMethod.POST)),
      Gen.alphaNumStr,
      submitterPartsGen[Id],
      Gen.alphaNumStr) { (d, payload, sp, expectedBody) =>
      import sp._

      val destination = d.copy(payload = Option(payload))

      val expectedResponse = mock[HttpResponse]

      val processorModel = HandlebarsTemplateProcessorModel("{}")

      (templateProcessor
        .apply(_: String, _: HandlebarsTemplateProcessorModel))
        .expects(payload, processorModel)
        .returning(expectedBody)

      (RealHandlebarsHttpApiSubmitter
        .selectHttpClient[Id](destination.profile, des, mdg)
        .postJson(_: String, _: String)(_: HeaderCarrier))
        .expects(destination.uri, expectedBody, hc)
        .returning(expectedResponse)

      submitter.apply(destination, processorModel) shouldBe expectedResponse
    }
  }

  it should "make a POST request when there is no payload" in {
    forAll(DestinationGen.handlebarsHttpApiGen.map(_.copy(method = HttpMethod.POST)), submitterPartsGen[Id]) {
      (d, sp) =>
        import sp._

        val destination = d.copy(payload = None)

        val expectedResponse = mock[HttpResponse]

        val processorModel = HandlebarsTemplateProcessorModel("{}")

        (RealHandlebarsHttpApiSubmitter
          .selectHttpClient[Id](destination.profile, des, mdg)
          .postJson(_: String, _: String)(_: HeaderCarrier))
          .expects(destination.uri, "", hc)
          .returning(expectedResponse)

        submitter.apply(destination, processorModel) shouldBe expectedResponse
    }
  }

  case class SubmitterParts[F[_]](
    submitter: HandlebarsHttpApiSubmitter[F],
    des: HttpClient[F],
    mdg: HttpClient[F],
    templateProcessor: HandlebarsTemplateProcessor)

  private def submitterPartsGen[F[_]]: Gen[SubmitterParts[F]] = {
    val desHttpClient = mock[HttpClient[F]]
    val mdgIntegrationFrameworkHttpClient = mock[HttpClient[F]]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]

    val submitter =
      new RealHandlebarsHttpApiSubmitter(desHttpClient, mdgIntegrationFrameworkHttpClient, handlebarsTemplateProcessor)

    SubmitterParts(submitter, desHttpClient, mdgIntegrationFrameworkHttpClient, handlebarsTemplateProcessor)
  }
}
