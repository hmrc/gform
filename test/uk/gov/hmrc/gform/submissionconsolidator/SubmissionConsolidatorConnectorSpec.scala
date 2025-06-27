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

package uk.gov.hmrc.gform.submissionconsolidator

import com.github.tomakehurst.wiremock.client.WireMock._
import com.typesafe.config.ConfigFactory
import org.apache.pekko.actor.ActorSystem
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{ Millis, Seconds, Span }
import play.api.Configuration
import play.api.libs.json.Json
import play.api.libs.ws.ahc.AhcWSClient
import uk.gov.hmrc.gform.WiremockSupport
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.http.client.{ HttpClientV2, HttpClientV2Impl }
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global

class SubmissionConsolidatorConnectorSpec
    extends AnyFlatSpec with MockFactory with WiremockSupport with SCFormGen with ScalaFutures with Matchers {

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(Span(10, Seconds), Span(1, Millis))

  trait TestFixture {
    val form: SCForm = genForm.pureApply(Gen.Parameters.default, Seed(1))
    lazy val responseStatus: Int = 200
    lazy val responseBody: String = ""
    lazy val r: HttpResponse = HttpResponse(
      responseStatus,
      responseBody,
      Map.empty[String, Seq[String]]
    )
    private implicit val testActorSystem: ActorSystem = ActorSystem("test-actor-system")
    protected val servicesConfig: ServicesConfig = mock[ServicesConfig]

    private val httpClientV2: HttpClientV2 =
      new HttpClientV2Impl(
        wsClient = AhcWSClient(),
        testActorSystem,
        Configuration(ConfigFactory.load()),
        hooks = Seq.empty
      )

    stubFor(
      post(urlEqualTo("/submission-consolidator/form"))
        .willReturn(
          aResponse()
            .withStatus(responseStatus)
            .withBody(responseBody)
        )
    )
    val baseUrl = s"http://localhost:$wiremockPort"
    val submissionConsolidatorConnector =
      new SubmissionConsolidatorConnector(httpClientV2, baseUrl)
  }

  "sendForm" should "on success, return empty result" in new TestFixture {
    //given
    override lazy val responseStatus: Int = 200
    override lazy val responseBody: String = ""

    //when
    val future = submissionConsolidatorConnector.sendForm(form)(HeaderCarrier())

    //then
    whenReady(future) { result =>
      result shouldBe Right(())
      verify(
        postRequestedFor(urlEqualTo("/submission-consolidator/form"))
          .withRequestBody(equalTo(Json.toJson(form).toString))
      )
    }
  }

  it should "on error with valid error json in response, return error message" in new TestFixture {
    //given
    override lazy val responseStatus: Int = 400
    override lazy val responseBody: String =
      Json.toJson(SCError("ERROR_CODE", "Error message", List(SCFieldError("/path", "message")))).toString

    //when
    val future = submissionConsolidatorConnector.sendForm(form)(HeaderCarrier())

    //then
    whenReady(future) { result =>
      result shouldBe Left("code=ERROR_CODE, message=Error message, fieldErrors=[/path=message]")
      verify(
        postRequestedFor(urlEqualTo("/submission-consolidator/form"))
          .withRequestBody(equalTo(Json.toJson(form).toString))
      )
    }
  }

  it should "on error with non-json message in response, return error message" in new TestFixture {
    //given
    override lazy val responseStatus: Int = 500
    override lazy val responseBody: String = "Some error message"

    //when
    val future = submissionConsolidatorConnector.sendForm(form)(HeaderCarrier())

    //then
    whenReady(future) { result =>
      result shouldBe Left("Some error message")
      verify(
        postRequestedFor(urlEqualTo("/submission-consolidator/form"))
          .withRequestBody(equalTo(Json.toJson(form).toString))
      )
    }
  }

  it should "on error due to connection timeout, return error message" in new TestFixture {
    //given
    override lazy val responseStatus: Int = 200
    override lazy val responseBody: String = ""
    stopServer()

    //when
    val future = submissionConsolidatorConnector.sendForm(form)(HeaderCarrier())

    //then
    whenReady(future) { result =>
      result.isLeft shouldBe true
      result.swap
        .getOrElse("Incorrect result")
        .contains(
          s"POST of '$baseUrl/submission-consolidator/form' failed. Caused by: 'Connection refused:"
        ) shouldBe true
      startServer()
    }
  }
}
