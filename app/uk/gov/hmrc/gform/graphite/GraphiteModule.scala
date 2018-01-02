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

package uk.gov.hmrc.gform.graphite

import play.api.Application
import uk.gov.hmrc.gform.ApplicationModule

class GraphiteModule(applicationModule: ApplicationModule) {

  val graphite = new Graphite(applicationModule.configuration).onStart(configurationApp)

  // To avoid circular dependency when creating ReactiveMongoComponentImpl and Graphite we will provide them this artificial
  // application. It is ok to do so since both of them are using mainly provided configuration.
  private lazy val configurationApp = new Application() {
    def actorSystem = applicationModule.actorSystem

    def classloader = applicationModule.environment.classLoader

    def configuration = applicationModule.configuration

    def errorHandler = applicationModule.httpErrorHandler

    implicit def materializer = applicationModule.materializer

    def mode = applicationModule.environment.mode

    def path = applicationModule.environment.rootPath

    def requestHandler = applicationModule.httpRequestHandler

    def stop() = applicationModule.applicationLifecycle.stop()
  }
}
