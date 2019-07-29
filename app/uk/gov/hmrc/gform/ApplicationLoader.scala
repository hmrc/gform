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

package uk.gov.hmrc.gform

import cats.instances.future._
import play.api.ApplicationLoader.Context
import play.api._
import play.api.http._
import play.api.i18n.I18nComponents
import play.api.inject.{ Injector, SimpleInjector }
import play.api.mvc.EssentialFilter
import play.api.routing.Router
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.dms.DmsModule
import uk.gov.hmrc.gform.email.EmailModule
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.form.{ FormModule, FormService }
import uk.gov.hmrc.gform.formtemplate.FormTemplateModule
import uk.gov.hmrc.gform.graphite.GraphiteModule
import uk.gov.hmrc.gform.metrics.MetricsModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorModule
import uk.gov.hmrc.gform.playcomponents.{ PlayComponents, PlayComponentsModule }
import uk.gov.hmrc.gform.save4later.{ Save4Later, Save4LaterModule }
import uk.gov.hmrc.gform.submission.{ DestinationAuditModule, SubmissionModule }
import uk.gov.hmrc.gform.submission.handlebars.HandlebarsHttpApiModule
import uk.gov.hmrc.gform.testonly.TestOnlyModule
import uk.gov.hmrc.gform.time.TimeModule
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.gform.obligation.ObligationModule

import scala.concurrent.ExecutionContext

class ApplicationLoader extends play.api.ApplicationLoader {
  def load(context: Context): Application = {
    LoggerConfigurator(context.environment.classLoader).foreach {
      _.configure(context.environment)
    }
    new ApplicationModule(context).application
  }
}

class ApplicationModule(context: Context) extends BuiltInComponentsFromContext(context) with I18nComponents { self =>

  Logger.info(s"Starting microservice GFORM}")

  implicit val ec: ExecutionContext = play.api.libs.concurrent.Execution.defaultContext

  private val akkaModule = new AkkaModule(materializer, actorSystem)
  protected val playComponents = new PlayComponents(context, self)

  private val metricsModule = new MetricsModule(playComponents, akkaModule)
  protected val configModule = new ConfigModule(playComponents)
  protected val auditingModule = new AuditingModule(configModule, akkaModule, playComponents)
  protected lazy val wSHttpModule = new WSHttpModule(auditingModule, configModule, playComponents)
  private val emailModule = new EmailModule(configModule, wSHttpModule)
  private val timeModule = new TimeModule
  val fileUploadModule = new FileUploadModule(configModule, wSHttpModule, timeModule)
  private val mongoModule = new MongoModule(playComponents)
  val formTemplateModule = new FormTemplateModule(mongoModule)
  protected lazy val shortLivedCacheModule = new Save4LaterModule(configModule, wSHttpModule)
  lazy val pdfGeneratorModule = new PdfGeneratorModule(configModule, wSHttpModule)

  lazy val save4later =
    new Save4Later(shortLivedCacheModule.shortLivedCache)

  lazy val formService = new FormService(save4later, fileUploadModule.fileUploadService)

  lazy val formModule =
    new FormModule(configModule, formTemplateModule, fileUploadModule, formService)

  val validationModule = new ValidationModule(wSHttpModule, configModule)

  private val destinationAuditModule = new DestinationAuditModule(
    configModule,
    mongoModule
  )

  private val handlebarsModule = new HandlebarsHttpApiModule(wSHttpModule, configModule)
  private val submissionModule =
    new SubmissionModule(
      configModule,
      mongoModule,
      pdfGeneratorModule,
      formService,
      formTemplateModule,
      fileUploadModule,
      wSHttpModule,
      timeModule,
      emailModule,
      handlebarsModule)
  private val dmsModule = new DmsModule(fileUploadModule, pdfGeneratorModule, configModule.appConfig)
  private val obligationModule = new ObligationModule(wSHttpModule, configModule)
  private val testOnlyModule =
    new TestOnlyModule(
      mongoModule,
      wSHttpModule,
      configModule,
      playComponents,
      formService,
      formTemplateModule.formTemplateService,
      Some(fileUploadModule.fileUploadService))
  private val graphiteModule = new GraphiteModule(self)

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
    destinationAuditModule,
    validationModule,
    dmsModule,
    obligationModule
  )

  override lazy val httpErrorHandler: HttpErrorHandler = playComponentsModule.errorHandler
  override lazy val httpRequestHandler: HttpRequestHandler = playComponentsModule.httpRequestHandler
  override lazy val httpFilters: Seq[EssentialFilter] = playComponentsModule.httpFilters

  override def router: Router = playComponentsModule.router

  lazy val customInjector: Injector = new SimpleInjector(injector) + playComponents.ahcWSComponents.wsApi
  override lazy val application = new DefaultApplication(
    environment,
    applicationLifecycle,
    customInjector,
    configuration,
    httpRequestHandler,
    httpErrorHandler,
    actorSystem,
    materializer)

  Logger.info(
    s"Microservice GFORM started in mode ${environment.mode} at port ${application.configuration.getString("http.port")}")
}

object ApplicationModuleHelper {

  def tweak(applicationModule: ApplicationModule): Unit = {

    // Since core libraries are using deprecated play.api.libs.ws.WS we need to add wsApi into injector

  }
}
