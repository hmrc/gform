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

package uk.gov.hmrc.gform.support

import akka.actor.{ActorSystem, Terminated}
import akka.stream.ActorMaterializer
import org.scalatestplus.play.{BaseOneServerPerSuite, FakeApplicationFactory}
import play.api.ApplicationLoader.Context
import play.api.libs.json.Writes
import play.api.libs.ws.WSRequest
import play.api.libs.ws.ahc.AhcWSClient
import play.api._
import play.api.{Application, Environment}
import uk.gov.hmrc.gform.ApplicationModule
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.Future

/**
 * This spec provides running play application for every test.
 */
trait ITSpec extends ITSpecBase with BaseOneServerPerSuite with /*TODO MongoSpecSupport with */ FakeApplicationFactory {

  override def fakeApplication(): Application = application
  lazy val wsclient = WSHttp

  private lazy val mongoDbName: String = "test-" + this.getClass.getSimpleName
  private lazy val env: Environment = Environment.simple(mode = Mode.Test)
  private lazy val configurationOverridings = Map(
    "mongodb.uri" -> s"mongodb://localhost:27017/$mongoDbName")

  private lazy val context: Context = ApplicationLoader.createContext(env)

  private lazy val applicationModule = new ApplicationModule(context) {
    override lazy val httpFilters = Nil
  }
  private lazy val application = applicationModule.application

  implicit lazy val hc = HeaderCarrier()

  lazy val baseUrl = s"http://localhost:${port}"

  lazy val gformConnector = new GformConnector(wsclient, s"$baseUrl/gform")

  /**
    * Stubbed WSHttp which responses always with the same HttpResponse. Use it for test purposes
    */
  class StubbedWSHttp(response: HttpResponse) extends WSHttp {
    override def doGet(url: String)(implicit hc: HeaderCarrier): Future[HttpResponse] = Future.successful(response)
    override def doPost[A](url: String, body: A, headers: Seq[(String, String)])(
      implicit rds: Writes[A],
      hc: HeaderCarrier) =
      Future.successful(response)
    override def doFormPost(url: String, body: Map[String, Seq[String]])(implicit hc: HeaderCarrier) =
      Future.successful(response)
    override def doPostString(url: String, body: String, headers: Seq[(String, String)])(implicit hc: HeaderCarrier) =
      Future.successful(response)
    override def doEmptyPost[A](url: String)(implicit hc: HeaderCarrier) = Future.successful(response)
    //TODO: PUT, PATCH, DELETE
  }

  //If you want to use WSHttp outside play app you must provide your WSClient. Otherwise it blows up.
  //See https://github.com/hmrc/http-verbs/issues/60
  //Don't use it on production ('ws.close()' logic is missing)
  object TestWSHttp extends WSHttp {
    override def buildRequest[A](url: String)(implicit hc: HeaderCarrier): WSRequest = ws.url(url)
    private implicit lazy val s: ActorSystem = ActorSystem()
    private implicit lazy val mat: ActorMaterializer = ActorMaterializer()
    private lazy val ws = AhcWSClient()(mat)

    def stop(): Future[Terminated] = s.terminate()
  }
}
