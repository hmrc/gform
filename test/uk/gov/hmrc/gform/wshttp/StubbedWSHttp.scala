/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.wshttp

import akka.actor.{ ActorSystem, Terminated }
import akka.stream.ActorMaterializer
import play.api.libs.json.Writes
import play.api.libs.ws.WSRequest
import play.api.libs.ws.ahc.AhcWSClient
import uk.gov.hmrc.play.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.Future

/**
 * Stubbed WSHttp which responses always with the same HttpResponse. Use it for test purposes
 */
class StubbedWSHttp(response: HttpResponse) extends WSHttp {
  override def doGet(url: String)(implicit hc: HeaderCarrier): Future[HttpResponse] = Future.successful(response)
  override def doPost[A](url: String, body: A, headers: Seq[(String, String)])(implicit rds: Writes[A], hc: HeaderCarrier) = Future.successful(response)
  override def doFormPost(url: String, body: Map[String, Seq[String]])(implicit hc: HeaderCarrier) = Future.successful(response)
  override def doPostString(url: String, body: String, headers: Seq[(String, String)])(implicit hc: HeaderCarrier) = Future.successful(response)
  override def doEmptyPost[A](url: String)(implicit hc: HeaderCarrier) = Future.successful(response)
  //TODO: PUT, PATCH, DELETE
}

//If you want to use WSHttp outside play app you must provide your WSClient. Otherwise it blows up.
//See https://github.com/hmrc/http-verbs/issues/60
//Don't use it on production ('ws.close()' logic is missing)
/*
object TestWSHttp extends WSHttp {
  override def buildRequest[A](url: String)(implicit hc: HeaderCarrier): WSRequest = ws.url(url)
  private implicit lazy val s: ActorSystem = ActorSystem()
  private implicit lazy val mat: ActorMaterializer = ActorMaterializer()
  private lazy val ws = AhcWSClient()(mat)

  def stop(): Future[Terminated] = s.terminate()
}*/
