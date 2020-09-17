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

package uk.gov.hmrc.gform

import cats.instances.future._
import play.api.ApplicationLoader.Context
import play.api._
import play.api.http._
import play.api.i18n.I18nComponents
import play.api.inject.{ Injector, SimpleInjector }
import play.api.mvc.EssentialFilter
import play.api.routing.Router
import play.api.libs.ws.ahc.AhcWSComponents
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.allowedlist.AllowedListModule
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.dms.DmsModule
import uk.gov.hmrc.gform.email.EmailModule
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.form.{ FormModule, FormService }
import uk.gov.hmrc.gform.formmetadata.FormMetadataModule
import uk.gov.hmrc.gform.formtemplate.FormTemplateModule
import uk.gov.hmrc.gform.graphite.GraphiteModule
import uk.gov.hmrc.gform.metrics.MetricsModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.notifier.NotifierModule
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorModule
import uk.gov.hmrc.gform.playcomponents.{ ErrorHandler, PlayComponents, PlayComponentsModule }
import uk.gov.hmrc.gform.save4later.{ Save4Later, Save4LaterModule }
import uk.gov.hmrc.gform.submission.SubmissionModule
import uk.gov.hmrc.gform.submission.handlebars.HandlebarsHttpApiModule
import uk.gov.hmrc.gform.testonly.TestOnlyModule
import uk.gov.hmrc.gform.time.TimeModule
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.gform.obligation.ObligationModule
import uk.gov.hmrc.gform.submission.destinations.DestinationModule
import uk.gov.hmrc.gform.submissionconsolidator.SubmissionConsolidatorModule
import uk.gov.hmrc.play.bootstrap.config.AppName

import scala.concurrent.Future

class ApplicationLoader extends play.api.ApplicationLoader {
  def load(context: Context): Application = {
    LoggerConfigurator(context.environment.classLoader).foreach {
      _.configure(context.environment)
    }
    new ApplicationModule(context).application
  }
}

class ApplicationModule(context: Context)
    extends BuiltInComponentsFromContext(context) with AhcWSComponents with I18nComponents { self =>

  val appName = AppName.fromConfiguration(configuration)

  Logger.info(s"Starting microservice $appName")

  private val akkaModule = new AkkaModule(materializer, actorSystem)
  protected val playComponents = new PlayComponents(context, self, self)

  protected val configModule = new ConfigModule(configuration, playComponents, controllerComponents, appName)
  private val metricsModule = new MetricsModule(configModule, playComponents, akkaModule, executionContext)
  protected val auditingModule = new AuditingModule(configModule, akkaModule)
  protected val wSHttpModule = new WSHttpModule(auditingModule, configModule, playComponents)
  private val notifierModule = new NotifierModule(configModule)
  private val emailModule = new EmailModule(configModule, wSHttpModule, notifierModule)
  private val timeModule = new TimeModule
  val fileUploadModule = new FileUploadModule(configModule, wSHttpModule, timeModule, akkaModule)
  private val mongoModule = new MongoModule(playComponents)
  val formTemplateModule = new FormTemplateModule(controllerComponents, mongoModule)
  val allowedListModule = new AllowedListModule(controllerComponents, mongoModule)
  protected val shortLivedCacheModule = new Save4LaterModule(configModule, wSHttpModule)
  val pdfGeneratorModule = new PdfGeneratorModule(configModule, wSHttpModule)

  val formMetadaModule = new FormMetadataModule(mongoModule)

  val save4later =
    new Save4Later(shortLivedCacheModule.shortLivedCache)

  val formService: FormService[Future] =
    new FormService(
      save4later,
      fileUploadModule.fileUploadService,
      formTemplateModule.formTemplateService,
      formMetadaModule.formMetadataService
    )

  val formModule =
    new FormModule(configModule, formTemplateModule, fileUploadModule, formService)

  val destinationModule =
    new DestinationModule(configModule, mongoModule, formModule, fileUploadModule, formMetadaModule)

  val validationModule = new ValidationModule(wSHttpModule, configModule)

  private val handlebarsModule = new HandlebarsHttpApiModule(wSHttpModule, configModule)

  private val submissionConsolidatorModule = new SubmissionConsolidatorModule(wSHttpModule, formModule, configModule)

  private val submissionModule =
    new SubmissionModule(
      configModule,
      mongoModule,
      pdfGeneratorModule,
      formModule,
      formTemplateModule,
      fileUploadModule,
      wSHttpModule,
      timeModule,
      emailModule,
      submissionConsolidatorModule,
      handlebarsModule,
      destinationModule,
      notifierModule
    )

  private val dmsModule =
    new DmsModule(fileUploadModule, pdfGeneratorModule, configModule.appConfig, controllerComponents)
  private val obligationModule = new ObligationModule(wSHttpModule, configModule)
  private val testOnlyModule =
    new TestOnlyModule(
      mongoModule,
      wSHttpModule,
      configModule,
      playComponents,
      formService,
      formTemplateModule.formTemplateService,
      destinationModule,
      controllerComponents
    )

  val graphiteModule =
    new GraphiteModule(environment, configuration, configModule.runMode, applicationLifecycle, metricsModule)

  override lazy val httpErrorHandler: HttpErrorHandler = new ErrorHandler(
    playComponents.context.environment,
    playComponents.context.initialConfiguration,
    playComponents.context.sourceMapper)

  val playComponentsModule = new PlayComponentsModule(
    playComponents,
    akkaModule,
    configModule,
    auditingModule,
    metricsModule,
    formModule,
    formTemplateModule,
    testOnlyModule,
    submissionModule,
    validationModule,
    dmsModule,
    obligationModule,
    emailModule,
    allowedListModule,
    httpErrorHandler
  )

  override lazy val httpRequestHandler: HttpRequestHandler = playComponentsModule.httpRequestHandler
  override lazy val httpFilters: Seq[EssentialFilter] = playComponentsModule.httpFilters

  override def router: Router = playComponentsModule.router

  val customInjector: Injector = new SimpleInjector(injector) + wsClient
  override lazy val application = new DefaultApplication(
    environment,
    applicationLifecycle,
    customInjector,
    configuration,
    requestFactory,
    httpRequestHandler,
    httpErrorHandler,
    actorSystem,
    materializer)

  Logger.info(
    s"Microservice $appName started in mode ${environment.mode} at port ${application.configuration.getOptional[String]("http.port")}")
}

object ApplicationModuleHelper {

  def tweak(applicationModule: ApplicationModule): Unit = {

    // Since core libraries are using deprecated play.api.libs.ws.WS we need to add wsApi into injector

  }
}
