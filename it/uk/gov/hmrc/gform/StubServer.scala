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

package uk.gov.hmrc.gform

import play.Mode
import play.api.ApplicationLoader.Context
import play.api._
import play.api.i18n.I18nComponents
import play.server.Server
import uk.gov.hmrc.gform.wshttp.{TestWSHttpIT, WSHttp, WSHttpModule}

class StubApplicationLoader extends play.api.ApplicationLoader {
  def load(context: Context): Application = {
    LoggerConfigurator(context.environment.classLoader).foreach {
      _.configure(context.environment)
    }
    new StubApplicationModule(context).application
  }
}

class StubApplicationModule(context: Context) extends BuiltInComponentsFromContext(context) with I18nComponents {
  self =>
  val module: ApplicationModule = new ApplicationModule(context) {
    override lazy val wSHttpModule = new WSHttpModule(auditingModule, configModule, playComponents) {
      override val auditableWSHttp: WSHttp = TestWSHttpIT
    }
  }
  override def router: routing.Router = module.router
}

trait StubServer {
  lazy val env: Environment = Environment.simple(mode = play.api.Mode.Test)
  val ctx = ApplicationLoader.createContext(env)
  val stubbedModule = new StubApplicationModule(ctx)
  lazy val port = 9199
  lazy val baseUrl = s"http://localhost:$port"

  Server.forRouter(stubbedModule.router.asJava, Mode.TEST, port)
}

object StubServer extends StubServer
