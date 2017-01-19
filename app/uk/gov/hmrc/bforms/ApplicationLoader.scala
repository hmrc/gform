/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms

import com.kenshoo.play.metrics.{ MetricsFilter, MetricsFilterImpl, MetricsImpl, MetricsController }
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import play.api.Logger
import play.api.http.HttpRequestHandler
import play.api.inject.{ Injector, SimpleInjector }
import play.api.{ Environment, LoggerConfigurator }
import play.api.http.{ DefaultHttpErrorHandler, DefaultHttpRequestHandler, HttpConfiguration, HttpErrorHandler }
import play.api.libs.ws.ahc.AhcWSComponents
import play.api.mvc.{ Handler, RequestHeader, Result }
import play.api.i18n.I18nComponents
import play.api.ApplicationLoader.Context
import play.api.{ BuiltInComponentsFromContext, BuiltInComponents, DefaultApplication }
import play.api.mvc.EssentialFilter
import play.api.routing.Router
import play.api.{ Application, Configuration }
import play.core.SourceMapper
import play.modules.reactivemongo.ReactiveMongoComponentImpl
import reactivemongo.api.DefaultDB
import scala.concurrent.Future
import uk.gov.hmrc.bforms.repositories.{ FormTemplateRepository, SchemaRepository, TestRepository }
import uk.gov.hmrc.bforms.controllers.{ Forms, FormTemplates, MicroserviceHelloWorld, Schemas }
import uk.gov.hmrc.bforms.repositories.{ SaveAndRetrieveRepositoryImpl, TestRepository }
import uk.gov.hmrc.bforms.controllers.{ MicroserviceHelloWorld, SaveAndRetrieveController }
import uk.gov.hmrc.bforms.services.FileUploadService
import uk.gov.hmrc.play.filters.{ NoCacheFilter, RecoveryFilter }
import uk.gov.hmrc.play.graphite.GraphiteConfig
import uk.gov.hmrc.play.audit.http.config.ErrorAuditingSettings
import uk.gov.hmrc.play.config.{ AppName, ControllerConfig, ServicesConfig }
import uk.gov.hmrc.play.auth.controllers.AuthParamsControllerConfig
import uk.gov.hmrc.play.audit.filters.AuditFilter
import uk.gov.hmrc.play.health.AdminController
import uk.gov.hmrc.play.http.logging.filters.LoggingFilter
import uk.gov.hmrc.play.auth.microservice.filters.AuthorisationFilter
import uk.gov.hmrc.play.microservice.bootstrap.JsonErrorHandling
import uk.gov.hmrc.bforms.connectors.{ FusConnector, FusFeConnector }
import uk.gov.hmrc.bforms.typeclasses.{ FusUrl, FusFeUrl, ServiceUrl }

class ApplicationLoader extends play.api.ApplicationLoader {
  def load(context: Context) = {
    LoggerConfigurator(context.environment.classLoader).foreach { _.configure(context.environment) }
    (new BuiltInComponentsFromContext(context) with ApplicationModule).application
  }
}

class CustomHttpRequestHandler(
    router: Router,
    httpErrorHandler: HttpErrorHandler,
    httpConfiguration: HttpConfiguration,
    httpFilters: Seq[EssentialFilter]
) extends DefaultHttpRequestHandler(router, httpErrorHandler, httpConfiguration, httpFilters: _*) {
  override def routeRequest(request: RequestHeader): Option[Handler] = {
    router.handlerFor(request).orElse {
      Some(request.path).filter(_.endsWith("/")).flatMap(p => router.handlerFor(request.copy(path = p.dropRight(1))))
    }
  }
}

class CustomErrorHandling(
    val auditConnector: MicroserviceAuditConnector.type,
    val appName: String,
    environment: Environment,
    configuration: Configuration,
    sourceMapper: Option[SourceMapper] = None,
    router: => Option[Router] = None
) extends DefaultHttpErrorHandler(environment, configuration, sourceMapper, router) with JsonErrorHandling with ErrorAuditingSettings {

  override def onBadRequest(request: RequestHeader, error: String): Future[Result] = {
    super.onBadRequest(request, error)
  }

  override def onNotFound(request: RequestHeader, message: String): Future[Result] = {
    super.onHandlerNotFound(request)
  }

  override def onServerError(request: RequestHeader, exception: Throwable): Future[Result] = {
    super.onError(request, exception)
  }
}

class Graphite(configuration: Configuration) extends GraphiteConfig {
  override def microserviceMetricsConfig(implicit app: Application): Option[Configuration] = configuration.getConfig(s"microservice.metrics")
}

trait ApplicationModule extends BuiltInComponents
    with AhcWSComponents
    with I18nComponents
    with AppName
    with ServicesConfig { self =>

  override lazy val runModeConfiguration = configuration
  override lazy val appNameConfiguration = configuration
  override lazy val mode = environment.mode

  Logger.info(s"Starting microservice : $appName : in mode : ${environment.mode}")

  new Graphite(configuration).onStart(configurationApp)

  override lazy val httpErrorHandler: HttpErrorHandler = new CustomErrorHandling(auditConnector, appName, environment, configuration, sourceMapper, Some(router))

  override lazy val httpRequestHandler: HttpRequestHandler = new CustomHttpRequestHandler(router, httpErrorHandler, httpConfiguration, httpFilters)

  override lazy val application: Application = new DefaultApplication(environment, applicationLifecycle, customInjector,
    configuration, httpRequestHandler, httpErrorHandler, actorSystem, materializer)

  // To avoid circular dependency when creating ReactiveMongoComponentImpl and Graphite we will provide them this artificial
  // application. It is ok to do so since both of them are using mainly provided configuration.
  lazy val configurationApp = new Application() {
    def actorSystem = self.actorSystem
    def classloader = self.environment.classLoader
    def configuration = self.configuration
    def errorHandler = self.httpErrorHandler
    implicit def materializer = self.materializer
    def mode = self.environment.mode
    def path = self.environment.rootPath
    def requestHandler = self.httpRequestHandler
    def stop() = self.applicationLifecycle.stop()
  }

  // Don't use uk.gov.hmrc.play.graphite.GraphiteMetricsImpl as it won't allow hot reload due to overridden onStop() method
  lazy val metrics = new MetricsImpl(applicationLifecycle, configuration)

  val metricsFilter: MetricsFilter = new MetricsFilterImpl(metrics)

  override lazy val httpFilters: Seq[EssentialFilter] = Seq(
    metricsFilter,
    microserviceAuditFilter,
    loggingFilter,
    authFilter,
    NoCacheFilter,
    RecoveryFilter
  )

  lazy val reactiveMongoComponent = new ReactiveMongoComponentImpl(configurationApp, applicationLifecycle)

  lazy val db: () => DefaultDB = reactiveMongoComponent.mongoConnector.db

  lazy val testRepository = new TestRepository()(db)
  lazy val saveAndRetrieveRespository = new SaveAndRetrieveRepositoryImpl()(db)

  lazy implicit val schemaRepository = new SchemaRepository()(db)
  lazy implicit val formTemplateRepository = new FormTemplateRepository()(db)
  implicit val fusUrl = new ServiceUrl[FusUrl] {
    val url = baseUrl("file-upload")
  }

  implicit val fusFeUrl = new ServiceUrl[FusFeUrl] {
    val url = baseUrl("file-upload-frontend")
  }

  lazy val microserviceHelloWorld = new MicroserviceHelloWorld(testRepository, fileUploadService, saveAndRetrieveRespository)
  lazy val saveAndRetrieveController = new SaveAndRetrieveController(messagesApi)(saveAndRetrieveRespository)

  lazy val formTemplates = new FormTemplates()

  lazy val forms = new Forms()

  lazy val schemas = new Schemas()

  // We need to create explicit AdminController and provide it into injector so Runtime DI could be able
  // to find it when endpoints in health.Routes are being called
  lazy val adminController = new AdminController(configuration)

  // Since core libraries are using deprecated play.api.libs.ws.WS we need to add wsApi into injector
  lazy val customInjector: Injector = new SimpleInjector(injector) + adminController + wsApi

  lazy val healthRoutes: health.Routes = health.Routes

  lazy val metricsController = new MetricsController(metrics)

  lazy val appRoutes = new app.Routes(httpErrorHandler, microserviceHelloWorld, saveAndRetrieveController, forms, formTemplates, schemas)

  override lazy val router: Router = new prod.Routes(httpErrorHandler, appRoutes, healthRoutes, metricsController)

  lazy val fusConnector = new FusConnector()
  lazy val fusFeConnector = new FusFeConnector()

  lazy val fileUploadService = new FileUploadService(fusConnector, fusFeConnector)

  object ControllerConfiguration extends ControllerConfig {
    lazy val controllerConfigs = configuration.underlying.as[Config]("controllers")
  }

  object AuthParamsControllerConfiguration extends AuthParamsControllerConfig {
    lazy val controllerConfigs = ControllerConfiguration.controllerConfigs
  }

  object MicroserviceAuditFilter extends AuditFilter {
    override val appName = self.appName
    override def mat = materializer
    override val auditConnector = self.auditConnector
    override def controllerNeedsAuditing(controllerName: String) = ControllerConfiguration.paramsForController(controllerName).needsAuditing
  }

  object MicroserviceLoggingFilter extends LoggingFilter {
    override def mat = materializer
    override def controllerNeedsLogging(controllerName: String) = ControllerConfiguration.paramsForController(controllerName).needsLogging
  }

  object MicroserviceAuthFilter extends AuthorisationFilter {
    override def mat = materializer
    override lazy val authParamsConfig = AuthParamsControllerConfiguration
    override lazy val authConnector = MicroserviceAuthConnector
    override def controllerNeedsAuth(controllerName: String): Boolean = ControllerConfiguration.paramsForController(controllerName).needsAuth
  }

  val auditConnector = MicroserviceAuditConnector

  val loggingFilter = MicroserviceLoggingFilter

  val microserviceAuditFilter = MicroserviceAuditFilter
  val authFilter = MicroserviceAuthFilter
}
