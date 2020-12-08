/*
 * Copyright 2020 HM Revenue & Customs
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

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, GivenWhenThen, Matchers}
import org.scalatestplus.play.{BaseOneServerPerSuite, FakeApplicationFactory}
import play.api.libs.ws.StandaloneWSClient
import play.api.libs.ws.ahc.StandaloneAhcWSClient
import play.api.{Application, Environment}
import uk.gov.hmrc.gform.ApplicationLoader
import scala.concurrent.ExecutionContext.Implicits.global

trait ITSpec
    extends MongoDBSupport with FlatSpecLike with GivenWhenThen with Matchers with BaseOneServerPerSuite with BeforeAndAfterAll
    with FakeApplicationFactory with ScalaFutures {

  implicit val system: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val settingsOverride: Map[String, String] = Map(
    "auditing.enabled" -> "false"
  ) ++ mongoSettings

  lazy val baseUrl: String = s"http://localhost:$port/gform"

  override def fakeApplication(): Application = {
    val context =
      play.api.ApplicationLoader.createContext(environment = Environment.simple(), initialSettings = settingsOverride)
    new ApplicationLoader().load(context)
  }

  override protected def afterAll(): Unit = {
    mongoConnector.db().drop().futureValue
    app.stop().futureValue
    system.terminate()
    ()
  }

  val wsClient: StandaloneWSClient = StandaloneAhcWSClient()
}
