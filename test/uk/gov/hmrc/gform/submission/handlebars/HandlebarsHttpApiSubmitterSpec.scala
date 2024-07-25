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

package uk.gov.hmrc.gform.submission.handlebars

import cats.MonadError
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import uk.gov.hmrc.gform.sharedmodel.PdfContent
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.{ Possible, Spec, possibleMonadError }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, PrimitiveGen }
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.wshttp.HttpClient
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

class HandlebarsHttpApiSubmitterSpec extends Spec with ScalaCheckDrivenPropertyChecks {
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  "A GET destination" should "make a GET request, applying the template to the URI" in {
    forAll(
      destinationGen(HttpMethod.GET),
      submitterPartsGen[Possible],
      PrimitiveGen.urlContextPathGen
    ) { (destination, sp, expectedUri) =>
      val processorModel = HandlebarsTemplateProcessorModel.empty
      val expectedResponse = mock[HttpResponse]

      sp.expectTemplateProcessorApplication(destination.uri, processorModel, TemplateType.Plain, expectedUri)
        .expectHttpGet(destination.profile, expectedUri, Right(expectedResponse))

      sp.submitter.apply(destination, HandlebarsTemplateProcessorModel.empty, tree(processorModel)) shouldBe Right(
        expectedResponse
      )
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
      val destination = d.copy(payload = Option(payload), payloadType = TemplateType.Plain)
      val processorModel = HandlebarsTemplateProcessorModel.empty
      val expectedResponse = mock[HttpResponse]

      sp.expectTemplateProcessorApplication(destination.uri, processorModel, TemplateType.Plain, expectedUri)
        .expectTemplateProcessorApplication(payload, processorModel, destination.payloadType, expectedBody)
        .expectHttpPostJson(destination.profile, expectedBody, expectedUri, Right(expectedResponse))

      sp.submitter.apply(destination, HandlebarsTemplateProcessorModel.empty, tree(processorModel)) shouldBe Right(
        expectedResponse
      )
    }
  }

  it should "make a POST request when there is no payload" in {
    forAll(destinationGen(HttpMethod.POST), submitterPartsGen[Possible], PrimitiveGen.urlContextPathGen) {
      (d, sp, expectedUri) =>
        val destination = d.copy(payload = None, payloadType = TemplateType.Plain)
        val processorModel = HandlebarsTemplateProcessorModel.empty
        val expectedResponse = mock[HttpResponse]

        sp.expectTemplateProcessorApplication(destination.uri, processorModel, TemplateType.Plain, expectedUri)
          .expectHttpPostJson(destination.profile, "", expectedUri, Right(expectedResponse))

        sp.submitter.apply(destination, HandlebarsTemplateProcessorModel.empty, tree(processorModel)) shouldBe Right(
          expectedResponse
        )
    }
  }

  it should "make multiple POSTs request when there is a JsArray payload and  multiRequestPayload = true " in {
    val payload1 = """|  {
                      |    "properties": [
                      |      {
                      |        "name": "dob",
                      |        "value": "2000-12-13"
                      |      }
                      |    ]
                      |  }""".stripMargin
    val payload2 = """|  {
                      |    "properties": [
                      |      {
                      |        "name": "dob",
                      |        "value": "1999-11-15"
                      |      }
                      |    ]
                      |  }""".stripMargin
    val payload = "[" + payload1 + "," + payload2 + "]"
    val d = destinationGen(HttpMethod.POST).sample.get
    val httpClient = mock[HttpClient[Possible]]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]

    val submitter =
      new RealHandlebarsHttpApiSubmitter(
        Map(ProfileName("foo") -> httpClient),
        handlebarsTemplateProcessor
      )

    val destination = d.copy(payload = Option(payload), payloadType = TemplateType.Plain, multiRequestPayload = true)
    val processorModel = HandlebarsTemplateProcessorModel.empty
    val expectedResponse = mock[HttpResponse]

    (httpClient
      .post(_: String, _: String)(_: HeaderCarrier))
      .expects(where { (url, body, hc) =>
        true
      })
      .returning(Right(expectedResponse))
      .twice()

    (handlebarsTemplateProcessor.apply _)
      .expects(payload1.filterNot(_.isWhitespace), *, *, *)
      .returning("foo")
      .once()
    (handlebarsTemplateProcessor.apply _)
      .expects(payload2.filterNot(_.isWhitespace), *, *, *)
      .returning("foo")
      .once()
    (handlebarsTemplateProcessor.apply _)
      .expects(d.uri, *, *, *)
      .returning("foo")
      .once()

    submitter
      .apply(destination, HandlebarsTemplateProcessorModel.empty, tree(processorModel)) shouldBe Right(
      expectedResponse
    )
  }

  it should "make multiple POSTs request until the failure when there is a JsArray payload and  multiRequestPayload = true " in {
    val payload1 = """|  {
                      |    "properties": [
                      |      {
                      |        "name": "dob",
                      |        "value": "2000-12-13"
                      |      }
                      |    ]
                      |  }""".stripMargin
    val payload2 = """|  {
                      |    "properties": [
                      |      {
                      |        "name": "dob",
                      |        "value": "1999-11-15"
                      |      }
                      |    ]
                      |  }""".stripMargin

    val payload3 = """|  {
                      |    "properties": [
                      |      {
                      |        "name": "dob",
                      |        "value": "1998-10-11"
                      |      }
                      |    ]
                      |  }""".stripMargin

    val payload = "[" + payload1 + "," + payload2 + "," + payload3 + "]"
    val d = destinationGen(HttpMethod.POST).sample.get
    val httpClient = mock[HttpClient[Possible]]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]

    val submitter =
      new RealHandlebarsHttpApiSubmitter(
        Map(ProfileName("foo") -> httpClient),
        handlebarsTemplateProcessor
      )

    val destination = d.copy(payload = Option(payload), payloadType = TemplateType.Plain, multiRequestPayload = true)
    val processorModel = HandlebarsTemplateProcessorModel.empty
    val expectedResponse = mock[HttpResponse]

    (() => expectedResponse.toString).expects().returning("response").anyNumberOfTimes()
    (httpClient
      .post(_: String, _: String)(_: HeaderCarrier))
      .expects(where { (_, body, _) =>
        body === "bad input"
      })
      .returning(Left("error response"))
      .once()

    (httpClient
      .post(_: String, _: String)(_: HeaderCarrier))
      .expects(where { (url, body, hc) =>
        true
      })
      .returning(Right(expectedResponse))
      .once()

    (handlebarsTemplateProcessor.apply _)
      .expects(payload1.filterNot(_.isWhitespace), *, *, *)
      .returning("foo")
      .once()
    (handlebarsTemplateProcessor.apply _)
      .expects(payload2.filterNot(_.isWhitespace), *, *, *)
      .returning("bad input")
      .once()

    (handlebarsTemplateProcessor.apply _)
      .expects(payload3.filterNot(_.isWhitespace), *, *, *)
      .returning("foo")
      .once()

    (handlebarsTemplateProcessor.apply _)
      .expects(d.uri, *, *, *)
      .returning("foo")
      .once()

    submitter
      .apply(destination, HandlebarsTemplateProcessorModel.empty, tree(processorModel)) shouldBe Left("error response")
  }

  it should "make one POST request when there is a JsArray payload multiRequestPayload = false " in {
    val payload1 = """|  {
                      |    "properties": [
                      |      {
                      |        "name": "dob",
                      |        "value": "2000-12-13"
                      |      }
                      |    ]
                      |  }""".stripMargin
    val payload2 = """|  {
                      |    "properties": [
                      |      {
                      |        "name": "dob",
                      |        "value": "1999-11-15"
                      |      }
                      |    ]
                      |  }""".stripMargin
    val payload = "[" + payload1 + "," + payload2 + "]"
    val d = destinationGen(HttpMethod.POST).sample.get
    val httpClient = mock[HttpClient[Possible]]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]

    val submitter =
      new RealHandlebarsHttpApiSubmitter(
        Map(ProfileName("foo") -> httpClient),
        handlebarsTemplateProcessor
      )

    val destination = d.copy(payload = Option(payload), payloadType = TemplateType.Plain, multiRequestPayload = false)
    val processorModel = HandlebarsTemplateProcessorModel.empty
    val expectedResponse = mock[HttpResponse]

    (httpClient
      .post(_: String, _: String)(_: HeaderCarrier))
      .expects(where { (url, body, hc) =>
        true
      })
      .returning(Right(expectedResponse))
      .once()

    (handlebarsTemplateProcessor.apply _)
      .expects(
        where { (actualPayload, _, _, _) =>
          actualPayload.filterNot(_.isWhitespace) === payload.filterNot(_.isWhitespace)
        }
      )
      .returning("foo")
      .once()
    (handlebarsTemplateProcessor.apply _)
      .expects(d.uri, *, *, *)
      .returning("foo")
      .once()

    submitter
      .apply(destination, HandlebarsTemplateProcessorModel.empty, tree(processorModel)) shouldBe Right(
      expectedResponse
    )
  }

  "A PUT destination" should "make a PUT request when there is a payload" in {
    forAll(
      destinationGen(HttpMethod.PUT),
      Gen.alphaNumStr,
      submitterPartsGen[Possible],
      PrimitiveGen.urlContextPathGen,
      Gen.alphaNumStr
    ) { (d, payload, sp, expectedUri, expectedBody) =>
      val destination = d.copy(payload = Option(payload), payloadType = TemplateType.Plain)
      val processorModel = HandlebarsTemplateProcessorModel.empty
      val expectedResponse = mock[HttpResponse]

      sp.expectTemplateProcessorApplication(destination.uri, processorModel, TemplateType.Plain, expectedUri)
        .expectTemplateProcessorApplication(payload, processorModel, destination.payloadType, expectedBody)
        .expectHttpPutJson(destination.profile, expectedBody, expectedUri, Right(expectedResponse))

      sp.submitter.apply(destination, HandlebarsTemplateProcessorModel.empty, tree(processorModel)) shouldBe Right(
        expectedResponse
      )
    }
  }

  it should "make a PUT request when there is no payload" in {
    forAll(destinationGen(HttpMethod.PUT), submitterPartsGen[Possible], PrimitiveGen.urlContextPathGen) {
      (d, sp, expectedUri) =>
        val destination = d.copy(payload = None, payloadType = TemplateType.Plain)
        val processorModel = HandlebarsTemplateProcessorModel.empty
        val expectedResponse = mock[HttpResponse]

        sp.expectTemplateProcessorApplication(destination.uri, processorModel, TemplateType.Plain, expectedUri)
          .expectHttpPutJson(destination.profile, "", expectedUri, Right(expectedResponse))

        sp.submitter.apply(destination, HandlebarsTemplateProcessorModel.empty, tree(processorModel)) shouldBe Right(
          expectedResponse
        )
    }
  }

  it should "make multiple PUTs request when there is a JsArray payload and  multiRequestPayload = true " in {
    val payload1 = """|  {
                      |    "properties": [
                      |      {
                      |        "name": "dob",
                      |        "value": "2000-12-13"
                      |      }
                      |    ]
                      |  }""".stripMargin
    val payload2 = """|  {
                      |    "properties": [
                      |      {
                      |        "name": "dob",
                      |        "value": "1999-11-15"
                      |      }
                      |    ]
                      |  }""".stripMargin
    val payload = "[" + payload1 + "," + payload2 + "]"
    val d = destinationGen(HttpMethod.PUT).sample.get
    val httpClient = mock[HttpClient[Possible]]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]

    val submitter =
      new RealHandlebarsHttpApiSubmitter(
        Map(ProfileName("foo") -> httpClient),
        handlebarsTemplateProcessor
      )

    val destination = d.copy(payload = Option(payload), payloadType = TemplateType.Plain, multiRequestPayload = true)
    val processorModel = HandlebarsTemplateProcessorModel.empty
    val expectedResponse = mock[HttpResponse]

    (httpClient
      .put(_: String, _: String)(_: HeaderCarrier))
      .expects(where { (url, body, hc) =>
        true
      })
      .returning(Right(expectedResponse))
      .twice()

    (handlebarsTemplateProcessor.apply _)
      .expects(payload1.filterNot(_.isWhitespace), *, *, *)
      .returning("foo")
      .once()
    (handlebarsTemplateProcessor.apply _)
      .expects(payload2.filterNot(_.isWhitespace), *, *, *)
      .returning("foo")
      .once()
    (handlebarsTemplateProcessor.apply _)
      .expects(d.uri, *, *, *)
      .returning("foo")
      .once()

    submitter
      .apply(destination, HandlebarsTemplateProcessorModel.empty, tree(processorModel)) shouldBe Right(
      expectedResponse
    )
  }

  it should "make one PUT request when there is a JsArray payload multiRequestPayload = false " in {
    val payload1 = """|  {
                      |    "properties": [
                      |      {
                      |        "name": "dob",
                      |        "value": "2000-12-13"
                      |      }
                      |    ]
                      |  }""".stripMargin
    val payload2 = """|  {
                      |    "properties": [
                      |      {
                      |        "name": "dob",
                      |        "value": "1999-11-15"
                      |      }
                      |    ]
                      |  }""".stripMargin
    val payload = "[" + payload1 + "," + payload2 + "]"
    val d = destinationGen(HttpMethod.PUT).sample.get
    val httpClient = mock[HttpClient[Possible]]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]

    val submitter =
      new RealHandlebarsHttpApiSubmitter(
        Map(ProfileName("foo") -> httpClient),
        handlebarsTemplateProcessor
      )

    val destination = d.copy(payload = Option(payload), payloadType = TemplateType.Plain, multiRequestPayload = false)
    val processorModel = HandlebarsTemplateProcessorModel.empty
    val expectedResponse = mock[HttpResponse]

    (httpClient
      .put(_: String, _: String)(_: HeaderCarrier))
      .expects(where { (url, body, hc) =>
        true
      })
      .returning(Right(expectedResponse))
      .once()

    (handlebarsTemplateProcessor.apply _)
      .expects(
        where { (actualPayload, _, _, _) =>
          actualPayload.filterNot(_.isWhitespace) === payload.filterNot(_.isWhitespace)
        }
      )
      .returning("foo")
      .once()
    (handlebarsTemplateProcessor.apply _)
      .expects(d.uri, *, *, *)
      .returning("foo")
      .once()

    submitter
      .apply(destination, HandlebarsTemplateProcessorModel.empty, tree(processorModel)) shouldBe Right(
      expectedResponse
    )
  }

  case class SubmitterParts[F[_]](
    submitter: HandlebarsHttpApiSubmitter[F],
    httpClient: HttpClient[F],
    templateProcessor: HandlebarsTemplateProcessor
  ) {

    def expectTemplateProcessorApplication(
      in: String,
      modelInFocus: HandlebarsTemplateProcessorModel,
      templateType: TemplateType,
      out: String
    ): SubmitterParts[F] = {
      (
        templateProcessor
          .apply(
            _: String,
            _: HandlebarsTemplateProcessorModel,
            _: FocussedHandlebarsModelTree,
            _: TemplateType
          )
        )
        .expects(in, HandlebarsTemplateProcessorModel.empty, rootFocussedTree(modelInFocus), templateType)
        .returning(out)

      this
    }

    def expectHttpGet(
      profile: ProfileName,
      expectedUri: String,
      expectedResponse: F[HttpResponse]
    ): SubmitterParts[F] = {
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
      expectedResponse: F[HttpResponse]
    ): SubmitterParts[F] = {
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
      expectedResponse: F[HttpResponse]
    ): SubmitterParts[F] = {
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
      new RealHandlebarsHttpApiSubmitter(
        Map(ProfileName("foo") -> httpClient),
        handlebarsTemplateProcessor
      )

    SubmitterParts(submitter, httpClient, handlebarsTemplateProcessor)
  }

  private def destinationGen(method: HttpMethod): Gen[Destination.HandlebarsHttpApi] =
    DestinationGen.handlebarsHttpApiGen.map(_.copy(method = method, profile = ProfileName("foo")))

  def rootFocussedTree(model: HandlebarsTemplateProcessorModel) =
    FocussedHandlebarsModelTree(tree(model), model)

  def tree(model: HandlebarsTemplateProcessorModel) =
    HandlebarsModelTree(
      FormId("someFormId"),
      submissionRef,
      null,
      PdfContent(""),
      None,
      StructuredFormValue.ObjectStructure(Nil),
      model
    )

}
