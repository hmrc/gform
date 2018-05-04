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

package uk.gov.hmrc.gform.config

import com.typesafe.config.{ ConfigFactory, Config => TypeSafeConfig }
import net.ceedubs.ficus.Ficus._
import play.api.Configuration
import play.api.Mode.Mode
import uk.gov.hmrc.gform.playcomponents.PlayComponents
import uk.gov.hmrc.gform.sharedmodel.config.ExposedConfig
import uk.gov.hmrc.play.auth.controllers.AuthParamsControllerConfig
import uk.gov.hmrc.play.config.{ ControllerConfig, ServicesConfig }

class ConfigModule(playComponents: PlayComponents) {

  val typesafeConfig: TypeSafeConfig = ConfigFactory.load()

  val appConfig: AppConfig = AppConfig.loadOrThrow(typesafeConfig)

  val exposedConfig: ExposedConfig = ExposedConfigHelper.exposedConfig(appConfig)

  val desConfig: DesConnectorConfig = pureconfig.loadConfigOrThrow[DesConnectorConfig]("microservice.services.etmp-hod")

  val emailConfig: EmailConnectorConfig =
    pureconfig.loadConfigOrThrow[EmailConnectorConfig]("microservice.services.email")

  val playConfiguration: Configuration = playComponents.context.initialConfiguration

  val serviceConfig: ServicesConfig = new ServicesConfig {
    //watch out!
    // ServicesConfig requires running play application so if we don't override these
    // we will experience 'Caused by: java.lang.RuntimeException: There is no started application'
    override protected def runModeConfiguration: Configuration = playConfiguration
    override protected def mode: Mode = playComponents.context.environment.mode
  }

  val controllerConfig: ControllerConfig = new ControllerConfig {
    lazy val controllerConfigs = typesafeConfig.as[TypeSafeConfig]("controllers")
  }

  val authParamsControllerConfig = new AuthParamsControllerConfig {
    lazy val controllerConfigs = controllerConfig.controllerConfigs
  }

  val configController = new ConfigController(this)

}

case class DesConnectorConfig(basePath: String, authorizationToken: String, environment: String)

case class EmailConnectorConfig(host: String, port: String)
