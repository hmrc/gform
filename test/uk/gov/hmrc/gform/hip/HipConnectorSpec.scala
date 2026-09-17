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

package uk.gov.hmrc.gform.hip

import izumi.reflect.Tag
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import play.api.http.Status.BAD_REQUEST
import play.api.libs.ws.{ BodyWritable, WSRequest }
import uk.gov.hmrc.gform.config.HipConnectorConfig
import uk.gov.hmrc.http.client.{ HttpClientV2, RequestBuilder, StreamHttpReads }
import uk.gov.hmrc.http.{ BadRequestException, HeaderCarrier, HttpReads, HttpResponse, NotFoundException }
import uk.gov.hmrc.gform.sharedmodel.sdes.CorrelationId

import java.net.URL
import scala.concurrent.{ ExecutionContext, Future }

class HipConnectorSpec extends AnyWordSpecLike with Matchers with ScalaFutures {

  private implicit val hc: HeaderCarrier = HeaderCarrier()
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val claimReference = "123456789A"

  private val hipConfig = HipConnectorConfig(
    basePath = "/hip",
    authorizationToken = "token",
    originatorId = "originator",
    correlationId = "correlation-id"
  )

  private def connector(response: HttpResponse): HipConnector =
    new HipConnector(
      new ResponseHttpClient(response),
      baseUrl = "http://localhost",
      hipConfig = hipConfig
    )

  "validateNIClaimReference" should {
    "map a failed validation 400 response to NotFoundException" in {
      val result = connector(
        HttpResponse(
          BAD_REQUEST,
          """{
            |  "origin": "HIP",
            |  "response": {
            |    "failures": [
            |      {
            |        "type": "path.refundClaimReference",
            |        "reason": "The request parameter path.refundClaimReference failed validation."
            |      },
            |      {
            |        "type": "path.refundClaimReference",
            |        "reason": "The request parameter path.refundClaimReference failed validation."
            |      }
            |    ]
            |  }
            |}
            |""".stripMargin
        )
      )
        .validateNIClaimReference("AA123456A", claimReference, CorrelationId("correlation-id"))

      val exception = result.failed.futureValue
      exception shouldBe a[NotFoundException]
      exception.getMessage shouldBe
        s"Validate NI Claim Reference returned identifier: $claimReference invalid"
    }

    "retain BadRequestException for other 400 responses" in {
      val result = connector(HttpResponse(BAD_REQUEST, "Claim request was malformed"))
        .validateNIClaimReference("AA123456A", claimReference, CorrelationId("correlation-id"))

      val exception = result.failed.futureValue
      exception shouldBe a[BadRequestException]
      exception.getMessage shouldBe
        s"Bad request response from validate NI Claim Reference for identifier: $claimReference"
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

  private class ResponseRequestBuilder(response: HttpResponse) extends RequestBuilder {
    override def execute[A](implicit rds: HttpReads[A], ec: ExecutionContext): Future[A] =
      Future.successful(rds.read("TEST", "http://localhost", response))

    override def setHeader(headers: (String, String)*): RequestBuilder = this
    override def transform(transform: WSRequest => WSRequest): RequestBuilder = this
    override def stream[A: StreamHttpReads](implicit ec: ExecutionContext): Future[A] = execute[A]
    override def withProxy: RequestBuilder = this
    override def withBody[B: BodyWritable: Tag](body: B)(implicit ec: ExecutionContext): RequestBuilder = this
  }
}
