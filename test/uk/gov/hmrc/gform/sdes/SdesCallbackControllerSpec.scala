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

package uk.gov.hmrc.gform.sdes

import org.apache.pekko.actor.{ ActorSystem, Scheduler }
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{ verify, when }
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.mockito.MockitoSugar
import play.api.http.Status.OK
import play.api.libs.json.Json
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.{ call, defaultAwaitTimeout, status, stubControllerComponents }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.sdes.{ CallBackNotification, NotificationStatus }
import uk.gov.hmrc.http.HeaderCarrier

import java.nio.charset.StandardCharsets
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, ExecutionContext, Future }

class SdesCallbackControllerSpec
    extends AnyWordSpec with Matchers with MockitoSugar with ScalaFutures with BeforeAndAfterAll {

  implicit private val system: ActorSystem = ActorSystem("sdes-callback-controller-spec")
  implicit private val ec: ExecutionContext = system.dispatcher
  implicit private val scheduler: org.apache.pekko.actor.Scheduler = system.scheduler

  override protected def afterAll(): Unit = {
    Await.result(system.terminate(), 10.seconds)
    super.afterAll()
  }

  "callback" should {
    "return OK when the callback is updated successfully" in new Fixture {
      when(mockSdesAlgebra.update(any[CallBackNotification])(any[HeaderCarrier], any[Scheduler]))
        .thenReturn(Future.successful(Right(())))

      val result: Future[Result] = call(controller.callback, request)

      status(result) shouldBe OK
      val notificationCaptor: ArgumentCaptor[CallBackNotification] =
        ArgumentCaptor.forClass(classOf[CallBackNotification])
      verify(mockSdesAlgebra).update(notificationCaptor.capture())(any[HeaderCarrier], any[Scheduler])
      notificationCaptor.getValue shouldBe notification
    }

    "return OK when updating the callback returns an unexpected state" in new Fixture {
      when(mockSdesAlgebra.update(any[CallBackNotification])(any[HeaderCarrier], any[Scheduler]))
        .thenReturn(Future.successful(Left(UnexpectedState("unable to update callback"))))

      val result: Future[Result] = call(controller.callback, request)

      status(result) shouldBe OK
      val notificationCaptor: ArgumentCaptor[CallBackNotification] =
        ArgumentCaptor.forClass(classOf[CallBackNotification])
      verify(mockSdesAlgebra).update(notificationCaptor.capture())(any[HeaderCarrier], any[Scheduler])
      notificationCaptor.getValue shouldBe notification
    }
  }

  trait Fixture {
    val mockSdesAlgebra: SdesAlgebra[Future] = mock[SdesAlgebra[Future]]
    val controller: SdesCallbackController = new SdesCallbackController(stubControllerComponents(), mockSdesAlgebra)
    val notification: CallBackNotification = CallBackNotification(
      NotificationStatus.FileProcessed,
      "file.xml",
      "correlation-id",
      Some("optional failure reason")
    )
    val notificationBody: Array[Byte] = Json.stringify(Json.toJson(notification)).getBytes(StandardCharsets.UTF_8)
    val request: FakeRequest[Array[Byte]] = FakeRequest("POST", "/sdes/callback")
      .withBody(notificationBody)
      .withHeaders("Content-Type" -> "application/json")
  }
}
