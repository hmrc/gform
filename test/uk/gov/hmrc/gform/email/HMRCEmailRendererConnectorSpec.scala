/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.email

import com.github.tomakehurst.wiremock.client.WireMock.{ aResponse, post, stubFor, urlEqualTo }
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{ Millis, Seconds, Span }
import org.scalatest.{ FlatSpecLike, Matchers }
import uk.gov.hmrc.gform.WiremockSupport
import uk.gov.hmrc.gform.common.WSHttpForWiremockSupport
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class HMRCEmailRendererConnectorSpec
    extends FlatSpecLike with Matchers with ScalaFutures with WiremockSupport with WSHttpForWiremockSupport {

  override implicit val patienceConfig = PatienceConfig(Span(2, Seconds), Span(500, Millis))

  trait TestFixture {
    implicit val hc: HeaderCarrier = HeaderCarrier()
    val hmrcEmailRendererConnector = new HMRCEmailRendererConnector(wsHttp, url)
  }

  "renderTemplate" should "return a Successful response, when downstream service returns 200" in new TestFixture {
    val emailRenderRequest = EmailRenderRequest("some-template-id", Map("param1" -> "value1"))
    stubFor(
      post(urlEqualTo(s"/templates/some-template-id"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody("")
        )
    )
    whenReady(hmrcEmailRendererConnector.renderTemplate(emailRenderRequest)) { result =>
      result shouldBe Successful
    }
  }

  it should "return a NotFound response, when downstream service returns 404" in new TestFixture {
    val emailRenderRequest = EmailRenderRequest("some-template-id", Map("param1" -> "value1"))
    stubFor(
      post(urlEqualTo(s"/templates/some-template-id"))
        .willReturn(
          aResponse()
            .withStatus(404)
        )
    )
    whenReady(hmrcEmailRendererConnector.renderTemplate(emailRenderRequest)) { result =>
      result shouldBe NotFound
    }
  }

  it should "return a ParametersNotFound response, when downstream service returns 400" in new TestFixture {
    val emailRenderRequest = EmailRenderRequest("some-template-id", Map.empty)
    stubFor(
      post(urlEqualTo(s"/templates/some-template-id"))
        .willReturn(
          aResponse()
            .withStatus(400)
            .withBody("""
                        |{
                        |   "status": "Rendering of template failed",
                        |   "reason": "key not found: param1"
                        |}
                        |""".stripMargin)
        )
    )
    whenReady(hmrcEmailRendererConnector.renderTemplate(emailRenderRequest)) { result =>
      result shouldBe ParametersNotFound("key not found: param1")
    }
  }

  it should "return a Unexpected response, when downstream service returns 500" in new TestFixture {
    val emailRenderRequest = EmailRenderRequest("some-template-id", Map.empty)
    stubFor(
      post(urlEqualTo(s"/templates/some-template-id"))
        .willReturn(
          aResponse()
            .withStatus(500)
            .withBody("Internal server error")
        )
    )
    whenReady(hmrcEmailRendererConnector.renderTemplate(emailRenderRequest)) { result =>
      result shouldBe Unexpected(
        "Unexpected response from hmrc-email-renderer render API [status=500, body=Internal server error]"
      )
    }
  }
}
