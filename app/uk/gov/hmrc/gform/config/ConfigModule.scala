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

package uk.gov.hmrc.gform.config

import com.typesafe.config.{ ConfigFactory, Config => TypeSafeConfig }
import play.api.Configuration
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.playcomponents.PlayComponents
import uk.gov.hmrc.gform.sharedmodel.config.ExposedConfig
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.ProfileName
import uk.gov.hmrc.http.logging.Authorization
import uk.gov.hmrc.play.audit.http.config.AuditingConfig
import uk.gov.hmrc.play.bootstrap.config.{ AuditingConfigProvider, ControllerConfigs, RunMode, ServicesConfig }

import scala.concurrent.ExecutionContext
import scala.util.Try
import pureconfig.generic.auto._
import pureconfig.ConfigSource
import uk.gov.hmrc.gform.notifier.NotifierConfig

class ConfigModule(
  val configuration: Configuration,
  playComponents: PlayComponents,
  val controllerComponents: ControllerComponents,
  appName: String)(implicit ec: ExecutionContext) {

  val typesafeConfig: TypeSafeConfig = ConfigFactory.load()

  val appConfig: AppConfig = AppConfig.loadOrThrow(typesafeConfig)

  val exposedConfig: ExposedConfig = ExposedConfigHelper.exposedConfig(appConfig)

  val desConfig: DesConnectorConfig =
    ConfigSource.default.at("microservice.services.etmp-hod").loadOrThrow[DesConnectorConfig]

  val emailConfig: EmailConnectorConfig =
    ConfigSource.default.at("microservice.services.email").loadOrThrow[EmailConnectorConfig]

  val controllerConfigs = ControllerConfigs.fromConfig(configuration)

  val runMode = new RunMode(configuration, playComponents.context.environment.mode)

  val serviceConfig: ServicesConfig = new ServicesConfig(configuration, runMode)

  val auditingConfig: AuditingConfig = new AuditingConfigProvider(configuration, runMode, appName).get()

  val notifierConfig: NotifierConfig =
    ConfigSource.default.at("microservice.services.notifier").loadOrThrow[NotifierConfig]

  val configController = new ConfigController(controllerComponents, this)

  object DestinationsServicesConfig extends ServicesConfig(configuration, runMode) {
    override protected lazy val rootServices = "microservice"
    override protected lazy val services = s"${runMode.env}.microservice"

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
        config(key).getOptional[String](k).map(v => (k, v))
      }

    private def getString(destinationServiceKey: String, key: String) =
      config(qualifiedDestinationServiceKey(destinationServiceKey)).getOptional[String](key)

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
      getString(destinationServiceKey, "base-path").getOrElse("")

    private def httpHeaders(destinationServiceKey: String): Map[String, String] =
      asStringStringMap(s"${qualifiedDestinationServiceKey(destinationServiceKey)}.http-headers")
        .getOrElse(Map.empty)

    private def profileName(destinationServiceKey: String): ProfileName =
      ProfileName(getString(destinationServiceKey, "name").getOrElse(destinationServiceKey))

    private def authToken(destinationServiceKey: String): Option[Authorization] =
      getString(destinationServiceKey, "authorization-token").map(a => Authorization(s"Bearer $a"))

    private val enableAuditKey = "enable-audit"
    def auditDestinations: Boolean = getConfBool(s"destination-services.$enableAuditKey", false)

    private val populateHandlebarsModelWithDocumentsKey = "populate-handlebars-model-with-documents"
    def populateHandlebarsModelWithDocuments: Boolean =
      getConfBool(s"destination-services.$populateHandlebarsModelWithDocumentsKey", false)

    import cats.instances.string._
    import cats.syntax.eq._
    def apply(): Map[ProfileName, ProfileConfiguration] =
      asMap("destination-services") { destinationServiceKey =>
        if (destinationServiceKey === "protocol" || destinationServiceKey === enableAuditKey || destinationServiceKey === populateHandlebarsModelWithDocumentsKey)
          None
        else {
          val name = profileName(destinationServiceKey)
          val configuration = ProfileConfiguration(
            name,
            baseUrl(destinationServiceKey) + basePath(destinationServiceKey),
            authToken(destinationServiceKey),
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
