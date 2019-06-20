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

import cats.MonadError
import org.scalacheck.Gen
import uk.gov.hmrc.gform.{ Possible, Spec, possibleMonadError }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, HandlebarsTemplateProcessorModel, HttpMethod, ProfileName }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, PrimitiveGen }
import uk.gov.hmrc.gform.wshttp.HttpClient
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

class HandlebarsHttpApiSubmitterSpec extends Spec {
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  "A GET destination" should "make a GET request, applying the template to the URI" in {
    forAll(
      destinationGen(HttpMethod.GET),
      submitterPartsGen[Possible],
      PrimitiveGen.urlContextPathGen
    ) { (destination, sp, expectedUri) =>
      val processorModel = HandlebarsTemplateProcessorModel("")
      val expectedResponse = mock[HttpResponse]

      sp.expectTemplateProcessorApplication(destination.uri, processorModel, expectedUri)
        .expectHttpGet(destination.profile, expectedUri, Right(expectedResponse))

      sp.submitter.apply(destination, processorModel) shouldBe Right(expectedResponse)
    }
  }

  "A POST destination" should "make a POST request when there is a payload" in {
    forAll(
      destinationGen(HttpMethod.POST),
      Gen.alphaNumStr,
      submitterPartsGen[Possible],
      PrimitiveGen.urlContextPathGen,
      Gen.alphaNumStr
    ) { (d, payload, sp, expectedUri, expectedBody) =>
      val destination = d.copy(payload = Option(payload))
      val processorModel = HandlebarsTemplateProcessorModel("")
      val expectedResponse = mock[HttpResponse]

      sp.expectTemplateProcessorApplication(destination.uri, processorModel, expectedUri)
        .expectTemplateProcessorApplication(payload, processorModel, expectedBody)
        .expectHttpPostJson(destination.profile, expectedBody, expectedUri, Right(expectedResponse))

      sp.submitter.apply(destination, processorModel) shouldBe Right(expectedResponse)
    }
  }

  it should "make a POST request when there is no payload" in {
    forAll(destinationGen(HttpMethod.POST), submitterPartsGen[Possible], PrimitiveGen.urlContextPathGen) {
      (d, sp, expectedUri) =>
        val destination = d.copy(payload = None)
        val processorModel = HandlebarsTemplateProcessorModel("")
        val expectedResponse = mock[HttpResponse]

        sp.expectTemplateProcessorApplication(destination.uri, processorModel, expectedUri)
          .expectHttpPostJson(destination.profile, "", expectedUri, Right(expectedResponse))

        sp.submitter.apply(destination, processorModel) shouldBe Right(expectedResponse)
    }
  }

  "A PUT destination" should "make a PUT request when there is a payload" in {
    forAll(
      destinationGen(HttpMethod.PUT),
      Gen.alphaNumStr,
      submitterPartsGen[Possible],
      PrimitiveGen.urlContextPathGen,
      Gen.alphaNumStr
    ) { (d, payload, sp, expectedUri, expectedBody) =>
      val destination = d.copy(payload = Option(payload))
      val processorModel = HandlebarsTemplateProcessorModel("")
      val expectedResponse = mock[HttpResponse]

      sp.expectTemplateProcessorApplication(destination.uri, processorModel, expectedUri)
        .expectTemplateProcessorApplication(payload, processorModel, expectedBody)
        .expectHttpPutJson(destination.profile, expectedBody, expectedUri, Right(expectedResponse))

      sp.submitter.apply(destination, processorModel) shouldBe Right(expectedResponse)
    }
  }

  it should "make a PUT request when there is no payload" in {
    forAll(destinationGen(HttpMethod.PUT), submitterPartsGen[Possible], PrimitiveGen.urlContextPathGen) {
      (d, sp, expectedUri) =>
        val destination = d.copy(payload = None)
        val processorModel = HandlebarsTemplateProcessorModel("")
        val expectedResponse = mock[HttpResponse]

        sp.expectTemplateProcessorApplication(destination.uri, processorModel, expectedUri)
          .expectHttpPutJson(destination.profile, "", expectedUri, Right(expectedResponse))

        sp.submitter.apply(destination, processorModel) shouldBe Right(expectedResponse)
    }
  }

  case class SubmitterParts[F[_]](
    submitter: HandlebarsHttpApiSubmitter[F],
    httpClient: HttpClient[F],
    templateProcessor: HandlebarsTemplateProcessor)(implicit me: MonadError[F, String]) {

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

    def expectHttpGet(
      profile: ProfileName,
      expectedUri: String,
      expectedResponse: F[HttpResponse]): SubmitterParts[F] = {
      (httpClient
        .get(_: String)(_: HeaderCarrier))
        .expects(expectedUri, hc)
        .returning(expectedResponse)

      this
    }

    def expectHttpPostJson(
      profile: ProfileName,
      expectedBody: String,
      expectedUri: String,
      expectedResponse: F[HttpResponse]): SubmitterParts[F] = {
      (httpClient
        .post(_: String, _: String)(_: HeaderCarrier))
        .expects(expectedUri, expectedBody, hc)
        .returning(expectedResponse)

      this
    }

    def expectHttpPutJson(
      profile: ProfileName,
      expectedBody: String,
      expectedUri: String,
      expectedResponse: F[HttpResponse]): SubmitterParts[F] = {
      (httpClient
        .put(_: String, _: String)(_: HeaderCarrier))
        .expects(expectedUri, expectedBody, hc)
        .returning(expectedResponse)

      this
    }
  }

  private def submitterPartsGen[F[_]](implicit me: MonadError[F, String]): Gen[SubmitterParts[F]] = {
    val httpClient = mock[HttpClient[F]]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]

    val submitter =
      new RealHandlebarsHttpApiSubmitter(Map(ProfileName("foo") -> httpClient), handlebarsTemplateProcessor)

    SubmitterParts(submitter, httpClient, handlebarsTemplateProcessor)
  }

  private def destinationGen(method: HttpMethod): Gen[Destination.HandlebarsHttpApi] =
    DestinationGen.handlebarsHttpApiGen.map(_.copy(method = method, profile = ProfileName("foo")))
}
