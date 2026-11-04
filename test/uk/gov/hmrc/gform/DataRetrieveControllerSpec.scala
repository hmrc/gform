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

package uk.gov.hmrc.gform

import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.mockito.MockitoSugar
import play.api.libs.json.{ JsValue, Json }
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.gform.des.{ DesController, DesService }
import uk.gov.hmrc.gform.hip.{ HipAlgebra, HipController, HipServiceAlgebra }
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, NotFound, ServiceCallResponse, ServiceResponse }
import uk.gov.hmrc.gform.sharedmodel.des.{ DesRegistrationRequest, DesRegistrationResponse, Organisation, UkAddress }
import uk.gov.hmrc.gform.sharedmodel.sdes.CorrelationId
import uk.gov.hmrc.http.{ HeaderCarrier, ServiceUnavailableException }

import scala.concurrent.{ ExecutionContext, Future }

class DataRetrieveControllerSpec extends AnyWordSpec with Matchers with MockitoSugar {
  implicit private val ec: ExecutionContext = ExecutionContext.global

  "HIP agentDetails" should {
    val responses: List[(ServiceCallResponse[JsValue], Int)] = List(
      ServiceResponse(Json.obj("agencyDetails" -> Json.obj("name" -> "Agency"))) -> OK,
      NotFound               -> NOT_FOUND,
      CannotRetrieveResponse -> SERVICE_UNAVAILABLE
    )
    responses.foreach { case (response, expectedStatus) =>
      s"return $expectedStatus for $response without changing the response envelope" in {
        val connector = mock[HipAlgebra[Future]]
        when(connector.lookupAgentDetails(any[String], CorrelationId(any[String]))(any[HeaderCarrier]))
          .thenReturn(Future.successful(response))
        val controller =
          new HipController(mock[HipServiceAlgebra[Future]], stubControllerComponents(), connector)
        val result =
          controller.agentDetails("arn")(FakeRequest().withHeaders("correlationId" -> "correlation-id"))
        status(result) shouldBe expectedStatus
        contentAsJson(result) shouldBe Json.toJson(response)
      }
    }

    List(new ServiceUnavailableException("upstream unavailable"), new RuntimeException("network failure")).foreach {
      error =>
        s"not return success for a failed upstream future: ${error.getClass.getSimpleName}" in {
          val connector = mock[HipAlgebra[Future]]
          when(connector.lookupAgentDetails(any[String], CorrelationId(any[String]))(any[HeaderCarrier]))
            .thenReturn(Future.failed(error))
          val controller =
            new HipController(mock[HipServiceAlgebra[Future]], stubControllerComponents(), connector)
          val result =
            controller.agentDetails("arn")(FakeRequest().withHeaders("correlationId" -> "correlation-id"))
          status(result) shouldBe SERVICE_UNAVAILABLE
        }
    }
  }

  "DES organisation" should {
    val success = DesRegistrationResponse(
      "safe-id",
      None,
      "sap-number",
      false,
      false,
      false,
      false,
      Organisation("Organisation", false, "company"),
      UkAddress("Street", None, None, None, None),
      None
    )
    val responses: List[(ServiceCallResponse[DesRegistrationResponse], Int)] = List(
      ServiceResponse(success) -> OK,
      NotFound                 -> NOT_FOUND,
      CannotRetrieveResponse   -> SERVICE_UNAVAILABLE
    )
    responses.foreach { case (response, expectedStatus) =>
      s"return $expectedStatus for $response without changing the response envelope" in {
        val service = mock[DesService[Future]]
        when(service.organisation(any[String], any[DesRegistrationRequest])(any[HeaderCarrier]))
          .thenReturn(Future.successful(response))
        val controller = new DesController(stubControllerComponents(), service)
        val request = FakeRequest().withBody(DesRegistrationRequest("CT", false, false))
        val result = controller.organisation("utr")(request)
        status(result) shouldBe expectedStatus
        contentAsJson(result) shouldBe Json.toJson(response)
      }
    }
  }
}
