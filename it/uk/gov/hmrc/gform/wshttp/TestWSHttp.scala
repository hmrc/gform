/*
 * Copyright 2018 HM Revenue & Customs
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
import play.api.libs.ws.WSRequest
import play.api.libs.ws.ahc.AhcWSClient
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

//If you want to use WSHttp outside play app you must provide your WSClient. Otherwise it blows up.
//See https://github.com/hmrc/http-verbs/issues/60
//Don't use it on production ('ws.close()' logic is missing)
//TODO: this is copy paste from 'test'. Maybe it's worth to extract common test configuration
object TestWSHttpIT extends WSHttp {
  override def buildRequest[A](url: String)(implicit hc: HeaderCarrier): WSRequest = ws.url(url)
  private implicit lazy val s: ActorSystem = ActorSystem()
  private implicit lazy val mat: ActorMaterializer = ActorMaterializer()
  private lazy val ws = AhcWSClient()(mat)

  def stop(): Future[Terminated] = s.terminate()
}
