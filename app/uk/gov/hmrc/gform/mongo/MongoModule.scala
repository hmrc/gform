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

package uk.gov.hmrc.gform.mongo

import java.io.File

import akka.actor.ActorSystem
import akka.stream.Materializer
import play.api.Mode.Mode
import play.api.http.{ HttpErrorHandler, HttpRequestHandler }
import play.api.{ Application, Configuration }
import play.modules.reactivemongo.ReactiveMongoComponentImpl
import reactivemongo.api.DefaultDB
import uk.gov.hmrc.gform.ApplicationModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.playcomponents.PlayComponents

import scala.concurrent.Future

class MongoModule(playComponents: PlayComponents) {

  //WARN: this is because some genius passed application to the ReactiveMongoComponentImpl instead of only config
  //If you take a closer look you will see that ReactiveMongoComponentImpl uses only `configuration`
  private val configInApp = new Application {
    override def configuration: Configuration = playComponents.context.initialConfiguration
    override def path: File = ???
    override def classloader: ClassLoader = ???
    override def mode: Mode = ???
    override def actorSystem: ActorSystem = ???
    override implicit def materializer: Materializer = ???
    override def requestHandler: HttpRequestHandler = ???
    override def errorHandler: HttpErrorHandler = ???
    override def stop(): Future[_] = ???
  }

  val reactiveMongoComponent: ReactiveMongoComponentImpl =
    new ReactiveMongoComponentImpl(
      configInApp.configuration,
      playComponents.context.environment,
      playComponents.context.lifecycle)
  val mongo: () => DefaultDB = reactiveMongoComponent.mongoConnector.db

}
