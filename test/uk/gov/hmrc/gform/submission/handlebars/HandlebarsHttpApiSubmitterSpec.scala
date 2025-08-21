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

import izumi.reflect.Tag
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.ws.BodyWritable
import play.api.test.Helpers.{ await, defaultAwaitTimeout }
import uk.gov.hmrc.gform.sharedmodel.PdfContent
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.DestinationGen
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.submission.destinations.{ DestinationSubmissionInfo, DestinationSubmissionInfoGen }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads, HttpResponse }
import uk.gov.hmrc.http.client.RequestBuilder

import scala.concurrent.{ ExecutionContext, Future }

class HandlebarsHttpApiSubmitterSpec
    extends Spec with ScalaCheckDrivenPropertyChecks with DestinationSubmissionInfoGen {
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  private def submissionInfoGen: Gen[DestinationSubmissionInfo] =
    DestinationSubmissionInfoGen.destinationSubmissionInfoGen.map { si =>
      si.copy(submission =
        si.submission.copy(_id = si.submission._id.copy(formId = form._id), envelopeId = EnvelopeId("envId"))
      )
    }

  "A GET destination" should "make a GET request, applying the template to the URI" in {
    val destination = destinationGen(HttpMethod.GET).sample.get
    val submissionInfo = submissionInfoGen.sample.get
    val expectedUri = "test-uri"

    val mockRequestBuilder = mock[RequestBuilder]
    val buildRequest = mockFunction[ProfileName, EnvelopeId, String, HttpMethod, HeaderCarrier, RequestBuilder]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]
    val expectedResponse = mock[HttpResponse]

    val submitter = new RealHandlebarsHttpApiSubmitter(buildRequest, handlebarsTemplateProcessor)
    val processorModel = HandlebarsTemplateProcessorModel.empty

    (handlebarsTemplateProcessor.apply _)
      .expects(*, processorModel, *, *)
      .returning(expectedUri)
      .anyNumberOfTimes()

    buildRequest
      .expects(destination.profile, EnvelopeId("envId"), expectedUri, HttpMethod.GET, hc)
      .returning(mockRequestBuilder)
      .anyNumberOfTimes()

    (mockRequestBuilder
      .execute[HttpResponse](_: HttpReads[HttpResponse], _: ExecutionContext))
      .expects(*, *)
      .returning(Future.successful(expectedResponse))
      .anyNumberOfTimes()

    val result = await(submitter.apply(destination, processorModel, tree(processorModel), submissionInfo))
    result shouldBe expectedResponse

  }

  "A POST destination" should "make a POST request when there is a payload" in {
    val payload = "test-payload"
    val destination = destinationGen(HttpMethod.POST).sample.get.copy(
      payload = Some(payload),
      payloadType = TemplateType.Plain
    )
    val submissionInfo = submissionInfoGen.sample.get
    val expectedUri = "test-uri"
    val expectedBody = "processed-body"

    val mockRequestBuilder = mock[RequestBuilder]
    val buildRequest = mockFunction[ProfileName, EnvelopeId, String, HttpMethod, HeaderCarrier, RequestBuilder]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]
    val expectedResponse = mock[HttpResponse]

    val submitter = new RealHandlebarsHttpApiSubmitter(buildRequest, handlebarsTemplateProcessor)
    val processorModel = HandlebarsTemplateProcessorModel.empty

    (handlebarsTemplateProcessor.apply _)
      .expects(*, processorModel, *, *)
      .returning(expectedUri)

    (handlebarsTemplateProcessor.apply _)
      .expects(payload, processorModel, *, *)
      .returning(expectedBody)

    buildRequest
      .expects(destination.profile, EnvelopeId("envId"), expectedUri, HttpMethod.POST, hc)
      .returning(mockRequestBuilder)

    (mockRequestBuilder
      .setHeader(_: (String, String)))
      .expects(*)
      .returning(mockRequestBuilder)

    (mockRequestBuilder
      .withBody(_: String)(_: BodyWritable[String], _: Tag[String], _: ExecutionContext))
      .expects(expectedBody, *, *, *)
      .returning(mockRequestBuilder)

    (mockRequestBuilder
      .execute[HttpResponse](_: HttpReads[HttpResponse], _: ExecutionContext))
      .expects(*, *)
      .returning(Future.successful(expectedResponse))

    val result = await(submitter.apply(destination, processorModel, tree(processorModel), submissionInfo))
    result shouldBe expectedResponse

  }

  it should "make a POST request when there is no payload" in {
    val destination = destinationGen(HttpMethod.POST).sample.get.copy(
      payload = None,
      payloadType = TemplateType.Plain
    )
    val submissionInfo = submissionInfoGen.sample.get
    val expectedUri = "test-uri"

    val mockRequestBuilder = mock[RequestBuilder]
    val buildRequest = mockFunction[ProfileName, EnvelopeId, String, HttpMethod, HeaderCarrier, RequestBuilder]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]
    val expectedResponse = mock[HttpResponse]

    val submitter = new RealHandlebarsHttpApiSubmitter(buildRequest, handlebarsTemplateProcessor)
    val processorModel = HandlebarsTemplateProcessorModel.empty

    (handlebarsTemplateProcessor.apply _)
      .expects(*, processorModel, *, *)
      .returning(expectedUri)

    buildRequest
      .expects(destination.profile, EnvelopeId("envId"), expectedUri, HttpMethod.POST, hc)
      .returning(mockRequestBuilder)

    (mockRequestBuilder
      .setHeader(_: (String, String)))
      .expects(*)
      .returning(mockRequestBuilder)

    (mockRequestBuilder
      .withBody(_: String)(_: BodyWritable[String], _: Tag[String], _: ExecutionContext))
      .expects("", *, *, *)
      .returning(mockRequestBuilder)

    (mockRequestBuilder
      .execute[HttpResponse](_: HttpReads[HttpResponse], _: ExecutionContext))
      .expects(*, *)
      .returning(Future.successful(expectedResponse))

    val result = await(submitter.apply(destination, processorModel, tree(processorModel), submissionInfo))
    result shouldBe expectedResponse

  }

  "A PUT destination" should "make a PUT request when there is a payload" in {
    val payload = "test-payload"
    val destination = destinationGen(HttpMethod.PUT).sample.get.copy(
      payload = Some(payload),
      payloadType = TemplateType.JSON
    )
    val submissionInfo = submissionInfoGen.sample.get
    val expectedUri = "test-uri"
    val expectedBody = "processed-body"

    val mockRequestBuilder = mock[RequestBuilder]
    val buildRequest = mockFunction[ProfileName, EnvelopeId, String, HttpMethod, HeaderCarrier, RequestBuilder]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]
    val expectedResponse = mock[HttpResponse]

    val submitter = new RealHandlebarsHttpApiSubmitter(buildRequest, handlebarsTemplateProcessor)
    val processorModel = HandlebarsTemplateProcessorModel.empty

    (handlebarsTemplateProcessor.apply _)
      .expects(*, processorModel, *, *)
      .returning(expectedUri)

    (handlebarsTemplateProcessor.apply _)
      .expects(payload, processorModel, *, *)
      .returning(expectedBody)

    buildRequest
      .expects(destination.profile, EnvelopeId("envId"), expectedUri, HttpMethod.PUT, hc)
      .returning(mockRequestBuilder)

    (mockRequestBuilder
      .setHeader(_: (String, String)))
      .expects(*)
      .returning(mockRequestBuilder)

    (mockRequestBuilder
      .withBody(_: String)(_: BodyWritable[String], _: Tag[String], _: ExecutionContext))
      .expects(expectedBody, *, *, *)
      .returning(mockRequestBuilder)

    (mockRequestBuilder
      .execute[HttpResponse](_: HttpReads[HttpResponse], _: ExecutionContext))
      .expects(*, *)
      .returning(Future.successful(expectedResponse))

    val result = await(submitter.apply(destination, processorModel, tree(processorModel), submissionInfo))
    result shouldBe expectedResponse

  }

  it should "handle multiple requests when multiRequestPayload is true" in {
    val payload1 = """{"name": "dob", "value": "2000-12-13"}"""
    val payload2 = """{"name": "dob", "value": "1999-11-15"}"""
    val payload = s"[$payload1,$payload2]"

    val destination = destinationGen(HttpMethod.POST).sample.get.copy(
      payload = Some(payload),
      payloadType = TemplateType.JSON,
      multiRequestPayload = true
    )
    val submissionInfo = submissionInfoGen.sample.get
    val expectedUri = "test-uri"

    val mockRequestBuilder = mock[RequestBuilder]
    val buildRequest = mockFunction[ProfileName, EnvelopeId, String, HttpMethod, HeaderCarrier, RequestBuilder]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]
    val expectedResponse = mock[HttpResponse]

    val submitter = new RealHandlebarsHttpApiSubmitter(buildRequest, handlebarsTemplateProcessor)
    val processorModel = HandlebarsTemplateProcessorModel.empty

    (handlebarsTemplateProcessor.apply _)
      .expects(*, processorModel, *, *)
      .returning(expectedUri)

    (handlebarsTemplateProcessor.apply _)
      .expects(*, processorModel, *, *)
      .returning("processed1")

    (handlebarsTemplateProcessor.apply _)
      .expects(*, processorModel, *, *)
      .returning("processed2")

    buildRequest
      .expects(destination.profile, EnvelopeId("envId"), expectedUri, HttpMethod.POST, hc)
      .returning(mockRequestBuilder)
      .once()

    (mockRequestBuilder
      .setHeader(_: (String, String)))
      .expects(*)
      .returning(mockRequestBuilder)
      .twice()

    (mockRequestBuilder
      .withBody(_: String)(_: BodyWritable[String], _: Tag[String], _: ExecutionContext))
      .expects("processed1", *, *, *)
      .returning(mockRequestBuilder)

    (mockRequestBuilder
      .withBody(_: String)(_: BodyWritable[String], _: Tag[String], _: ExecutionContext))
      .expects("processed2", *, *, *)
      .returning(mockRequestBuilder)

    (mockRequestBuilder
      .execute[HttpResponse](_: HttpReads[HttpResponse], _: ExecutionContext))
      .expects(*, *)
      .returning(Future.successful(expectedResponse))
      .twice()

    val result = await(submitter.apply(destination, processorModel, tree(processorModel), submissionInfo))
    result shouldBe expectedResponse

  }

  private def destinationGen(method: HttpMethod): Gen[Destination.HandlebarsHttpApi] =
    DestinationGen.handlebarsHttpApiGen.map(_.copy(method = method, profile = ProfileName("foo")))

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
