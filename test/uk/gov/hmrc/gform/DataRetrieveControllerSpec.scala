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

import munit.FunSuite
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{ mock, when }
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

class DataRetrieveControllerSpec extends FunSuite {
  implicit private val ec: ExecutionContext = ExecutionContext.global

  private val hipResponses: List[(ServiceCallResponse[JsValue], Int)] = List(
    ServiceResponse(Json.obj("agencyDetails" -> Json.obj("name" -> "Agency"))) -> OK,
    NotFound               -> NOT_FOUND,
    CannotRetrieveResponse -> SERVICE_UNAVAILABLE
  )
  hipResponses.foreach { case (response, expectedStatus) =>
    test(s"HIP agentDetails returns $expectedStatus for $response without changing the response envelope") {
      val connector = mock(classOf[HipAlgebra[Future]])
      when(connector.lookupAgentDetails(any[String], CorrelationId(any[String]))(any[HeaderCarrier]))
        .thenReturn(Future.successful(response))
      val controller =
        new HipController(mock(classOf[HipServiceAlgebra[Future]]), stubControllerComponents(), connector)
      val result =
        controller.agentDetails("arn")(FakeRequest().withHeaders("correlationId" -> "correlation-id"))
      assertEquals(status(result), expectedStatus)
      assertEquals(contentAsJson(result), Json.toJson(response))
    }
  }

  List(new ServiceUnavailableException("upstream unavailable"), new RuntimeException("network failure")).foreach {
    error =>
      test(s"HIP agentDetails does not return success for a failed upstream future: ${error.getClass.getSimpleName}") {
        val connector = mock(classOf[HipAlgebra[Future]])
        when(connector.lookupAgentDetails(any[String], CorrelationId(any[String]))(any[HeaderCarrier]))
          .thenReturn(Future.failed(error))
        val controller =
          new HipController(mock(classOf[HipServiceAlgebra[Future]]), stubControllerComponents(), connector)
        val result =
          controller.agentDetails("arn")(FakeRequest().withHeaders("correlationId" -> "correlation-id"))
        assertEquals(status(result), SERVICE_UNAVAILABLE)
      }
  }

  private val success = DesRegistrationResponse(
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
  private val desResponses: List[(ServiceCallResponse[DesRegistrationResponse], Int)] = List(
    ServiceResponse(success) -> OK,
    NotFound                 -> NOT_FOUND,
    CannotRetrieveResponse   -> SERVICE_UNAVAILABLE
  )
  desResponses.foreach { case (response, expectedStatus) =>
    test(s"DES organisation returns $expectedStatus for $response without changing the response envelope") {
      val service = mock(classOf[DesService[Future]])
      when(service.organisation(any[String], any[DesRegistrationRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(response))
      val controller = new DesController(stubControllerComponents(), service)
      val request = FakeRequest().withBody(DesRegistrationRequest("CT", false, false))
      val result = controller.organisation("utr")(request)
      assertEquals(status(result), expectedStatus)
      assertEquals(contentAsJson(result), Json.toJson(response))
    }
  }
}
