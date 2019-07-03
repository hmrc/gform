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

import java.net.URLDecoder

import com.typesafe.config.{ ConfigFactory, Config => TypeSafeConfig }
import net.ceedubs.ficus.Ficus._
import play.api.Configuration
import play.api.Mode.Mode
import uk.gov.hmrc.gform.playcomponents.PlayComponents
import uk.gov.hmrc.gform.sharedmodel.config.ExposedConfig
import uk.gov.hmrc.play.auth.controllers.AuthParamsControllerConfig
import uk.gov.hmrc.play.config.{ ControllerConfig, ServicesConfig }
import pureconfig.generic.auto._
import uk.gov.hmrc.gform.sharedmodel.form.{ Approved, FormStatus, InProgress, Submitted }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.ProfileName
import uk.gov.service.notify.NotificationClient

import scala.util.Try // It is now necessary to import `pureconfig.generic.auto._` everywhere a config is loaded or written, even though IntelliJ sees this as unused, its still required

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

  object DestinationsServicesConfig extends ServicesConfig {
    override protected def mode: Mode = playComponents.context.environment.mode
    override protected def runModeConfiguration: Configuration = playConfiguration

    override protected lazy val rootServices = "microservice"
    override protected lazy val services = s"$env.microservice"

    private def qualifiedDestinationServiceKey(destinationServiceKey: String) =
      s"destination-services.$destinationServiceKey"

    private def asMap[K, V](key: String)(entry: String => Option[(K, V)]): Option[Map[K, V]] =
      Try {
        config(key)
      }.toOption.map {
        _.subKeys
          .flatMap(v => entry(v))
          .toMap
      }

    private def asStringStringMap(key: String): Option[Map[String, String]] =
      asMap(key) { k =>
        config(key).getString(k).map(v => (k, v))
      }

    private def getString(destinationServiceKey: String, key: String) =
      config(qualifiedDestinationServiceKey(destinationServiceKey)).getString(key)

    private def getConfString(destinationServiceKey: String, key: String, dflt: => String): String =
      getConfString(s"${qualifiedDestinationServiceKey(destinationServiceKey)}.$key", dflt)

    override def baseUrl(destinationServiceKey: String) = {
      val protocol =
        getConfString(
          destinationServiceKey,
          "protocol",
          getConfString(s"destination-services.protocol", defaultProtocol))
      val host =
        getConfString(
          destinationServiceKey,
          "host",
          throw new RuntimeException(
            s"Could not find config ${qualifiedDestinationServiceKey(destinationServiceKey)}.host"))
      val port = getConfInt(
        s"${qualifiedDestinationServiceKey(destinationServiceKey)}.port",
        throw new RuntimeException(
          s"Could not find config ${qualifiedDestinationServiceKey(destinationServiceKey)}.port")
      )
      s"$protocol://$host:$port"
    }

    private def basePath(destinationServiceKey: String): String =
      getString(destinationServiceKey, "basePath").getOrElse("")

    private def httpHeaders(destinationServiceKey: String): Map[String, String] =
      asStringStringMap(s"${qualifiedDestinationServiceKey(destinationServiceKey)}.httpHeaders")
        .getOrElse(Map.empty)
        .mapValues(URLDecoder.decode(_, "UTF-8"))

    private def profileName(destinationServiceKey: String): ProfileName =
      ProfileName(getString(destinationServiceKey, "name").getOrElse(destinationServiceKey))

    import cats.syntax.eq._
    import cats.instances.string._
    def apply(): Map[ProfileName, ProfileConfiguration] =
      asMap("destination-services") { destinationServiceKey =>
        if (destinationServiceKey === "protocol") None
        else {
          val name = profileName(destinationServiceKey)
          val configuration = ProfileConfiguration(
            name,
            baseUrl(destinationServiceKey) + basePath(destinationServiceKey),
            httpHeaders(destinationServiceKey)
          )

          Some((name, configuration))
        }
      }.getOrElse(Map.empty)
  }
}

case class DesConnectorConfig(basePath: String, authorizationToken: String, environment: String)

case class MdgIntegrationFrameworkConfig(basePath: String, authorizationToken: String)

case class EmailConnectorConfig(host: String, port: String)
