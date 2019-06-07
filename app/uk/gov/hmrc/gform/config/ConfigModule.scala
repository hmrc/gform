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

package uk.gov.hmrc.gform.config

import com.typesafe.config.{ ConfigFactory, Config => TypeSafeConfig }
import net.ceedubs.ficus.Ficus._
import play.api.Configuration
import play.api.Mode.Mode
import uk.gov.hmrc.gform.playcomponents.PlayComponents
import uk.gov.hmrc.gform.sharedmodel.config.ExposedConfig
import uk.gov.hmrc.gform.submission.handlebars.{ MdtpServiceConfiguration, MdtpServiceName }
import uk.gov.hmrc.play.auth.controllers.AuthParamsControllerConfig
import uk.gov.hmrc.play.config.{ ControllerConfig, ServicesConfig }
import pureconfig.generic.auto._
import uk.gov.service.notify.NotificationClient // It is now necessary to import `pureconfig.generic.auto._` everywhere a config is loaded or written, even though IntelliJ sees this as unused, its still required

class ConfigModule(playComponents: PlayComponents) {

  val typesafeConfig: TypeSafeConfig = ConfigFactory.load()

  val appConfig: AppConfig = AppConfig.loadOrThrow(typesafeConfig)

  val exposedConfig: ExposedConfig = ExposedConfigHelper.exposedConfig(appConfig)

  val desConfig: DesConnectorConfig = pureconfig.loadConfigOrThrow[DesConnectorConfig]("microservice.services.etmp-hod")

  lazy val mdtpServiceConfigs: Map[MdtpServiceName, MdtpServiceConfiguration] =
    Seq("tax-enrolments")
      .map(MdtpServiceName)
      .map { name =>
        (name, MdtpServicesConfig.mdtpServiceConfiguration(name))
      }
      .toMap

  val emailConfig: EmailConnectorConfig =
    pureconfig.loadConfigOrThrow[EmailConnectorConfig]("microservice.services.email")

  val env: String = typesafeConfig.getString("gform-environment")

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

  object MdtpServicesConfig extends ServicesConfig {
    override protected def runModeConfiguration: Configuration = playConfiguration
    override protected def mode: Mode = playComponents.context.environment.mode

    override protected lazy val rootServices = "microservice.destination-services"
    override protected lazy val services = s"$env.microservice.destination-services"

    private def authorizationToken(serviceName: String): Option[String] =
      config(serviceName).getString("authorizationToken")
    private def environment(serviceName: String): Option[String] = config(serviceName).getString("environment")

    private def basePath(serviceName: String): String = config(serviceName).getString("basePath").getOrElse("")

    def mdtpServiceConfiguration(name: MdtpServiceName): MdtpServiceConfiguration =
      MdtpServiceConfiguration(
        name,
        baseUrl(name.name) + basePath(name.name),
        authorizationToken(name.name),
        environment(name.name))
  }
}

trait OfstedNotificationConf {
  val ofstedNotification: OfstedNotificationConfig =
    pureconfig.loadConfigOrThrow[OfstedNotificationConfig]("ofsted.notifications")
  val notificationClient: NotificationClient = new NotificationClient(ofstedNotification.apiKey)
}

case class DesConnectorConfig(basePath: String, authorizationToken: String, environment: String)

case class MdgIntegrationFrameworkConfig(basePath: String, authorizationToken: String)

case class EmailConnectorConfig(host: String, port: String)

case class OfstedNotificationConfig(apiKey: String, template: String, phoneNumber: String, email: String)

case class EnvironmentConfig(env: String)
