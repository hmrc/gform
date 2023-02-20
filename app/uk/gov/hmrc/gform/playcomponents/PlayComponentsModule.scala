/*
 * Copyright 2023 HM Revenue & Customs
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

import org.slf4j.LoggerFactory
import play.api.http.HttpErrorHandler
import play.api.mvc.EssentialFilter
import play.api.routing.Router

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.formstatistics.FormStatisticsModule
import uk.gov.hmrc.gform.dblookup.DbLookupModule
import uk.gov.hmrc.gform.dms.DmsModule
import uk.gov.hmrc.gform.email.EmailModule
import uk.gov.hmrc.gform.employments.EmploymentsModule
import uk.gov.hmrc.gform.envelope.EnvelopeModule
import uk.gov.hmrc.gform.form.FormModule
import uk.gov.hmrc.gform.formtemplate.FormTemplateModule
import uk.gov.hmrc.gform.metrics.MetricsModule
import uk.gov.hmrc.gform.notificationbanner.NotificationBannerModule
import uk.gov.hmrc.gform.objectstore.ObjectStoreModule
import uk.gov.hmrc.gform.obligation.ObligationModule
import uk.gov.hmrc.gform.scheduler.SchedulerModule
import uk.gov.hmrc.gform.sdes.SdesModule
import uk.gov.hmrc.gform.submission.SubmissionModule
import uk.gov.hmrc.gform.testonly.TestOnlyModule
import uk.gov.hmrc.gform.translation.TranslationModule
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.gform.upscan.UpscanModule
import uk.gov.hmrc.play.bootstrap.filters.{ CacheControlConfig, CacheControlFilter, DefaultLoggingFilter, MDCFilter }
import uk.gov.hmrc.play.health.HealthController

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
  dmsModule: DmsModule,
  obligationModule: ObligationModule,
  employmentsModule: EmploymentsModule,
  emailModule: EmailModule,
  dbLookupModule: DbLookupModule,
  upscanModule: UpscanModule,
  errorHandler: HttpErrorHandler,
  formStatisticsModule: FormStatisticsModule,
  envelopeModule: EnvelopeModule,
  translationModule: TranslationModule,
  objectStoreModule: ObjectStoreModule,
  sdesModule: SdesModule,
  notificationBannerModule: NotificationBannerModule,
  schedulerModule: SchedulerModule
)(implicit ec: ExecutionContext) {
  private val logger = LoggerFactory.getLogger(getClass)

  private lazy val loggingFilter = new DefaultLoggingFilter(configModule.controllerConfigs)(akkaModule.materializer, ec)

  lazy val appRoutes: app.Routes = new app.Routes(
    errorHandler,
    formModule.formController,
    submissionModule.submissionController,
    submissionModule.formBundleController,
    formTemplateModule.formTemplatesController,
    configModule.configController,
    validationModule.validationController,
    dmsModule.dmsSubmissionController,
    obligationModule.obligationController,
    employmentsModule.employmentsController,
    emailModule.emailCodeVerificationController,
    dbLookupModule.dbLookupController,
    upscanModule.upscanController,
    formStatisticsModule.formStatisticsController,
    envelopeModule.envelopeController,
    objectStoreModule.objectStoreController,
    translationModule.translationController,
    notificationBannerModule.notificationBannerController,
    sdesModule.sdesCallbackController,
    sdesModule.sdesController
  )

  private val healthController =
    new HealthController(
      configModule.configuration,
      playComponents.context.environment,
      configModule.controllerComponents
    )

  lazy val prodRoutes: prod.Routes =
    new prod.Routes(errorHandler, appRoutes, healthController, metricsModule.metricsController)

  lazy val testOnlyDoNotUseInAppConfRoutes: testOnlyDoNotUseInAppConf.Routes =
    new testOnlyDoNotUseInAppConf.Routes(
      errorHandler,
      prodRoutes,
      testOnlyModule.testOnlyController,
      testOnlyModule.fUInterceptor,
      testOnlyModule.testOnlyObjectStoreController
    )

  lazy val router: Router = {
    val key = "play.http.router"
    val property = configModule.typesafeConfig.getString(key)
    property match {
      case null | "prod.Routes" =>
        logger.info("Using router with prod.Routes")
        prodRoutes
      case "testOnlyDoNotUseInAppConf.Routes" =>
        logger.info(s"Using router with $property")
        testOnlyDoNotUseInAppConfRoutes
      case _ =>
        logger.error(
          s"The option $key has unsupported value: $property. We support only prod.Routes and testOnlyDoNotUseInAppConf.Routes . Using prod.Routes ."
        )
        prodRoutes
    }
  }

  private lazy val cacheControlFilter = new CacheControlFilter(new CacheControlConfig(), akkaModule.materializer)

  lazy val httpFilters: Seq[EssentialFilter] = Seq(
    metricsModule.metricsFilter,
    auditingModule.microserviceAuditFilter,
    loggingFilter,
    cacheControlFilter,
    new MDCFilter(akkaModule.materializer, configModule.configuration, configModule.appConfig.appName)
  )

  lazy val httpRequestHandler =
    new CustomHttpRequestHandler(router, errorHandler, playComponents.builtInComponents.httpConfiguration, httpFilters)

}
