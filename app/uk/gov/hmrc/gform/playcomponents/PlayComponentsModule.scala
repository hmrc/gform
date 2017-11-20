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

package uk.gov.hmrc.gform.playcomponents

import akka.stream.Materializer
import play.api.Logger
import play.api.mvc.EssentialFilter
import play.api.routing.Router
import testOnlyDoNotUseInAppConf.Routes
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.form.FormModule
import uk.gov.hmrc.gform.formtemplate.FormTemplateModule
import uk.gov.hmrc.gform.metrics.MetricsModule
import uk.gov.hmrc.gform.submission.SubmissionModule
import uk.gov.hmrc.gform.testonly.TestOnlyModule
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.play.auth.controllers.AuthParamsControllerConfig
import uk.gov.hmrc.play.auth.microservice.connectors.AuthConnector
import uk.gov.hmrc.play.auth.microservice.filters.AuthorisationFilter
import uk.gov.hmrc.play.health.AdminController
import uk.gov.hmrc.play.microservice.filters.{LoggingFilter, NoCacheFilter}

class PlayComponentsModule(
  playComponents: PlayComponents,
  akkaModule: AkkaModule,
  configModule: ConfigModule,
  auditingModule: AuditingModule,
  metricsModule: MetricsModule,
  formModule: FormModule,
  formTemplateModule: FormTemplateModule,
  testOnlyModule: TestOnlyModule,
  submissionModule: SubmissionModule,
  validationModule: ValidationModule,
  authModule: AuthModule) {

  lazy val loggingFilter = new LoggingFilter {
    override def mat: Materializer = akkaModule.materializer
    override def controllerNeedsLogging(controllerName: String): Boolean = configModule.controllerConfig.paramsForController(controllerName).needsLogging
  }

  lazy val microserviceAuthConnector = new AuthConnector {
    override def authBaseUrl: String = configModule.serviceConfig.baseUrl("auth")
  }

  lazy val authFilter = new AuthorisationFilter {
    override def mat: Materializer = akkaModule.materializer
    override lazy val authParamsConfig: AuthParamsControllerConfig = configModule.authParamsControllerConfig
    override lazy val authConnector: AuthConnector = microserviceAuthConnector
    override def controllerNeedsAuth(controllerName: String): Boolean = configModule.controllerConfig.paramsForController(controllerName).needsAuth
  }

  lazy val appRoutes: app.Routes = new app.Routes(
    errorHandler,
    formModule.formController,
    submissionModule.submissionController,
    formTemplateModule.formTemplatesController,
    configModule.configController,
    validationModule.validationController,
    authModule.authController)

  val adminController = new AdminController(configModule.playConfiguration)

  lazy val prodRoutes: prod.Routes = new prod.Routes(
    errorHandler,
    appRoutes,
    adminController,
    metricsModule.metricsController)

  lazy val testOnlyDoNotUseInAppConfRoutes: testOnlyDoNotUseInAppConf.Routes = new testOnlyDoNotUseInAppConf.Routes(
    errorHandler,
    prodRoutes,
    testOnlyModule.testOnlyController,
    testOnlyModule.fUInterceptor)

  def router: Router = {
    val applicationRouterKey = "application.router"
    val applicationRouterProp = configModule.typesafeConfig.getString(applicationRouterKey)
    if (applicationRouterProp == null) {
      Logger.info("Using router with prod.routes")
      prodRoutes
    }
    if (applicationRouterProp == "testOnlyDoNotUseInAppConf.Routes") {
      Logger.info("Using router with testOnlyDoNotUseInAppConf.routes")
      testOnlyDoNotUseInAppConfRoutes
    } else {
      Logger.error(s"The option $applicationRouterKey has unsupported value: $applicationRouterProp. We support only 'testOnlyDoNotUseInAppConf.Routes'. Using 'prodRoutes'.")
      prodRoutes
    }
  }

  lazy val errorHandler = new ErrorHandler(
    playComponents.context.environment,
    playComponents.context.initialConfiguration,
    playComponents.context.sourceMapper)

  lazy val httpFilters: Seq[EssentialFilter] = Seq(
    metricsModule.metricsFilter,
    auditingModule.microserviceAuditFilter,
    loggingFilter,
    //    authFilter, it thorws exception instead of working ...
    NoCacheFilter)

  lazy val httpRequestHandler = new CustomHttpRequestHandler(
    router,
    errorHandler,
    playComponents.builtInComponents.httpConfiguration,
    httpFilters)

}
