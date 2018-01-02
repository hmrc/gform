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

import org.scalatestplus.play.{ BaseOneServerPerTest, FakeApplicationFactory }
import play.api.ApplicationLoader.Context
import play.api.libs.ws.WSClient
import play.api.{ Application, Configuration, Environment }
import play.core.DefaultWebCommands
import uk.gov.hmrc.gform.ApplicationModule
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.HeaderCarrier

/**
 * This spec provides running play application for every test.
 */
trait ITSpec extends ITSpecBase with BaseOneServerPerTest with /*TODO MongoSpecSupport with */ FakeApplicationFactory {

  override def fakeApplication(): Application = application

  lazy val wsclient = WSHttp

  private lazy val mongoDbName: String = "test-" + this.getClass.getSimpleName
  private lazy val env: Environment = Environment.simple()
  private lazy val configurationOverridings = Map(
    "mongodb.uri" -> s"mongodb://localhost:27017/$mongoDbName")
  private lazy val context: Context = Context(
    environment = env,
    sourceMapper = None,
    webCommands = new DefaultWebCommands(),
    initialConfiguration = Configuration.load(env))
  private lazy val applicationModule = new ApplicationModule(context) {
    override lazy val httpFilters = Nil
  }
  private lazy val application = applicationModule.application

  implicit lazy val hc = HeaderCarrier()

  lazy val baseUrl = s"http://localhost:${port}"

  lazy val gformConnector = new GformConnector(wsclient, s"$baseUrl/gform")
}
