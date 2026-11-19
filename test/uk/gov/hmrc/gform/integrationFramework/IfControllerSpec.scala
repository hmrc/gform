/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.integrationFramework

import izumi.reflect.Tag
import play.api.http.Status
import play.api.libs.json.Json
import play.api.libs.ws.{ BodyWritable, WSRequest }
import play.api.mvc.{ AnyContentAsEmpty, Result }
import play.api.test.FakeRequest
import play.api.test.Helpers.stubControllerComponents
import uk.gov.hmrc.gform.UnitSpec
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.http.client.{ HttpClientV2, RequestBuilder, StreamHttpReads }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads, HttpResponse }

import java.net.URL
import scala.concurrent.{ ExecutionContext, Future }

class IfControllerSpec extends UnitSpec {

  "verifyPodStatus" should {
    "return 200 OK with transformed response body when request is successful" in new Fixture {
      val podReference = "POD123456789012"
      val responseBody = Json.stringify(
        Json.obj(
          "matchFound"                -> true,
          "exporterCompanyName"       -> Some("Test Company"),
          "podReference"              -> Some("POD123456789012"),
          "currentRegistrationStatus" -> Some("VALID"),
          "registrationValidityPeriods" -> Json.arr(
            Json.obj(
              "status"    -> "VALID",
              "validFrom" -> "2020-01-01",
              "validTo"   -> Some("2025-12-31")
            )
          )
        )
      )

      val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/fta/verifystatus/POD123456789012")
        .withHeaders("correlationId" -> "correlation-123")

      val result: Future[Result] = controller(HttpResponse(200, responseBody)).verifyPodStatus(podReference)(request)

      status(result) shouldBe Status.OK
      contentAsString(result) should include("Test Company")
    }

    "return 400 Bad Request when request is malformed" in new Fixture {
      val podReference = "POD123456789012"
      val responseBody = """{"error":"Invalid request"}"""

      val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/fta/verifystatus/POD123456789012")
        .withHeaders("correlationId" -> "correlation-789")

      val result: Future[Result] = controller(HttpResponse(400, responseBody)).verifyPodStatus(podReference)(request)

      status(result) shouldBe Status.BAD_REQUEST
    }

    "return 500 Internal Server Error when IF service returns 500 with 400 statusCode in body" in new Fixture {
      val podReference = "POD123456789012"
      val responseBody = """{"error":"with statusCode: 400"}"""

      val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/fta/verifystatus/POD123456789012")
        .withHeaders("correlationId" -> "correlation-500")

      val result: Future[Result] = controller(HttpResponse(500, responseBody)).verifyPodStatus(podReference)(request)

      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "fail when correlationId header is missing" in new Fixture {
      val podReference = "POD123456789012"

      val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/fta/verifystatus/POD123456789012")

      val result = intercept[RuntimeException] {
        await(controller(HttpResponse(200, "{}")).verifyPodStatus(podReference)(request))
      }

      result.getMessage shouldBe "Request didn't contain correlationId header"
    }

    "return 200 OK with response when registration validity periods is empty" in new Fixture {
      val podReference = "POD123456789012"
      val responseBody = Json.stringify(
        Json.obj(
          "matchFound"                  -> true,
          "exporterCompanyName"         -> Some("Test Company"),
          "podReference"                -> Some("POD123456789012"),
          "currentRegistrationStatus"   -> Some("VALID"),
          "registrationValidityPeriods" -> Json.arr()
        )
      )

      val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/fta/verifystatus/POD123456789012")
        .withHeaders("correlationId" -> "correlation-empty-periods")

      val result: Future[Result] = controller(HttpResponse(200, responseBody)).verifyPodStatus(podReference)(request)

      status(result) shouldBe Status.OK
      contentAsString(result) should include("Test Company")
    }

    "return response body unchanged when status code is not 200" in new Fixture {
      val podReference = "POD123456789012"
      val responseBody = """{"error":"Not found"}"""

      val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/fta/verifystatus/POD123456789012")
        .withHeaders("correlationId" -> "correlation-404")

      val result: Future[Result] = controller(HttpResponse(404, responseBody)).verifyPodStatus(podReference)(request)

      status(result) shouldBe Status.NOT_FOUND
      contentAsString(result) shouldBe responseBody
    }

    "include Content-Type header as application/json in response" in new Fixture {
      val podReference = "POD123456789012"
      val responseBody = Json.stringify(Json.obj("matchFound" -> true))

      val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/fta/verifystatus/POD123456789012")
        .withHeaders("correlationId" -> "correlation-ct")

      val result: Future[Result] = controller(HttpResponse(200, responseBody)).verifyPodStatus(podReference)(request)

      status(result) shouldBe Status.OK
    }
  }

  trait Fixture {
    implicit val hc: HeaderCarrier = HeaderCarrier()
    implicit val ec: ExecutionContext = ExecutionContext.global

    private def testConfiguration: play.api.Configuration = {
      import com.typesafe.config.ConfigFactory
      // Load the default application.conf from the classpath
      val typesafeConf = ConfigFactory.load()
      play.api.Configuration(typesafeConf)
    }

    def controller(response: HttpResponse): IfController = {
      val mockHttpClient = new ResponseHttpClient(response)
      val wsHttpModule = new WSHttpModule(mockHttpClient)

      val configModule = new ConfigModule(testConfiguration, stubControllerComponents())

      new IfController(configModule, wsHttpModule, stubControllerComponents())
    }

    def controllerWithException(exception: Throwable): IfController = {
      val mockHttpClient = new ExceptionHttpClient(exception)
      val wsHttpModule = new WSHttpModule(mockHttpClient)

      val configModule = new ConfigModule(testConfiguration, stubControllerComponents())

      new IfController(configModule, wsHttpModule, stubControllerComponents())
    }
  }

  private class ResponseHttpClient(response: HttpResponse) extends HttpClientV2 {
    override def get(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = new ResponseRequestBuilder(response)
    override def post(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = new ResponseRequestBuilder(response)
    override def put(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = new ResponseRequestBuilder(response)
    override def delete(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = new ResponseRequestBuilder(response)
    override def head(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = new ResponseRequestBuilder(response)
    override protected def mkRequestBuilder(url: URL, method: String)(implicit hc: HeaderCarrier): RequestBuilder =
      new ResponseRequestBuilder(response)
  }

  private class ExceptionHttpClient(exception: Throwable) extends HttpClientV2 {
    override def get(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = new ExceptionRequestBuilder(exception)
    override def post(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = new ExceptionRequestBuilder(exception)
    override def put(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = new ExceptionRequestBuilder(exception)
    override def delete(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = new ExceptionRequestBuilder(exception)
    override def head(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = new ExceptionRequestBuilder(exception)
    override protected def mkRequestBuilder(url: URL, method: String)(implicit hc: HeaderCarrier): RequestBuilder =
      new ExceptionRequestBuilder(exception)
  }

  private class ResponseRequestBuilder(response: HttpResponse) extends RequestBuilder {
    override def execute[A](implicit rds: HttpReads[A], ec: ExecutionContext): Future[A] =
      Future.successful(rds.read("TEST", "http://localhost", response))

    override def setHeader(headers: (String, String)*): RequestBuilder = this
    override def transform(transform: WSRequest => WSRequest): RequestBuilder = this
    override def stream[A: StreamHttpReads](implicit ec: ExecutionContext): Future[A] = execute[A]
    override def withProxy: RequestBuilder = this
    override def withBody[B: BodyWritable: Tag](body: B)(implicit ec: ExecutionContext): RequestBuilder = this
  }

  private class ExceptionRequestBuilder(exception: Throwable) extends RequestBuilder {
    override def execute[A](implicit rds: HttpReads[A], ec: ExecutionContext): Future[A] =
      Future.failed(exception)

    override def setHeader(headers: (String, String)*): RequestBuilder = this
    override def transform(transform: WSRequest => WSRequest): RequestBuilder = this
    override def stream[A: StreamHttpReads](implicit ec: ExecutionContext): Future[A] = execute[A]
    override def withProxy: RequestBuilder = this
    override def withBody[B: BodyWritable: Tag](body: B)(implicit ec: ExecutionContext): RequestBuilder = this
  }
}
