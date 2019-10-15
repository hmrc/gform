/*
 * Copyright 2019 HM Revenue & Customs
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
import com.typesafe.config.Config
import play.api.libs.ws.WSClient
import uk.gov.hmrc.http.hooks.HttpHook
import uk.gov.hmrc.play.audit.http.connector.AuditConnector

import scala.concurrent.Future

//If you want to use WSHttp outside play app you must provide your WSClient. Otherwise it blows up.
//See https://github.com/hmrc/http-verbs/issues/60
//Don't use it on production ('ws.close()' logic is missing)
object TestWSHttp extends WSHttp {
  private implicit lazy val s: ActorSystem = ActorSystem()
  private implicit lazy val mat: ActorMaterializer = ActorMaterializer()

  def stop(): Future[Terminated] = s.terminate()

  override def auditConnector: AuditConnector = ???

  override def appName: String = ???

  override lazy val configuration: Option[Config] = None

  override val hooks: Seq[HttpHook] = Seq.empty

  override protected def actorSystem: ActorSystem = ???

  override def wsClient: WSClient = ???
}
