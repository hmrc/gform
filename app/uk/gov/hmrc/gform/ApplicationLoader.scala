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

package uk.gov.hmrc.gform

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
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.form.FormModule
import uk.gov.hmrc.gform.formtemplate.FormTemplateModule
import uk.gov.hmrc.gform.graphite.GraphiteModule
import uk.gov.hmrc.gform.metrics.MetricsModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorModule
import uk.gov.hmrc.gform.playcomponents.{ PlayComponents, PlayComponentsModule }
import uk.gov.hmrc.gform.save4later.Save4LaterModule
import uk.gov.hmrc.gform.submission.SubmissionModule
import uk.gov.hmrc.gform.testonly.TestOnlyModule
import uk.gov.hmrc.gform.time.TimeModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule

class ApplicationLoader extends play.api.ApplicationLoader {
  def load(context: Context) = {
    LoggerConfigurator(context.environment.classLoader).foreach {
      _.configure(context.environment)
    }
    new ApplicationModule(context).application
  }
}

class ApplicationModule(context: Context) extends BuiltInComponentsFromContext(context)
    with I18nComponents { self =>

  Logger.info(s"Starting microservice GFORM}")

  private val akkaModule = new AkkaModule(materializer, actorSystem)
  private val playComponents = new PlayComponents(context, self)

  private val metricsModule = new MetricsModule(playComponents, akkaModule)
  private val configModule = new ConfigModule(playComponents)
  private val auditingModule = new AuditingModule(configModule, akkaModule, playComponents)
  private val wSHttpModule = new WSHttpModule(auditingModule, configModule, playComponents)

  private val timeModule = new TimeModule
  private val fileUploadModule = new FileUploadModule(configModule, wSHttpModule, timeModule)
  private val mongoModule = new MongoModule(playComponents)
  private val formTemplateModule = new FormTemplateModule(mongoModule)
  private val shortLivedCacheModule = new Save4LaterModule(configModule, wSHttpModule)
  private val pdfGeneratorModule = new PdfGeneratorModule(configModule, wSHttpModule)

  private val formModule = new FormModule(mongoModule, shortLivedCacheModule, formTemplateModule, fileUploadModule)
  private val submissionModule = new SubmissionModule(mongoModule, pdfGeneratorModule, formModule, formTemplateModule, fileUploadModule, timeModule)
  private val testOnlyModule = new TestOnlyModule(mongoModule)
  private val graphiteModule = new GraphiteModule(self)

  private val playComponentsModule = new PlayComponentsModule(playComponents, akkaModule, configModule, auditingModule, metricsModule, formModule, formTemplateModule, testOnlyModule, submissionModule)

  override lazy val httpErrorHandler: HttpErrorHandler = playComponentsModule.errorHandler
  override lazy val httpRequestHandler: HttpRequestHandler = playComponentsModule.httpRequestHandler
  override lazy val httpFilters: Seq[EssentialFilter] = playComponentsModule.httpFilters

  override def router: Router = playComponentsModule.router

  lazy val customInjector: Injector = new SimpleInjector(injector) + playComponents.ahcWSComponents.wsApi
  override lazy val application = new DefaultApplication(environment, applicationLifecycle, customInjector, configuration, httpRequestHandler, httpErrorHandler, actorSystem, materializer)

  Logger.info(s"Microservice GFORM started in mode ${environment.mode} at port ${application.configuration.getString("http.port")}")
}

object ApplicationModuleHelper {

  def tweak(applicationModule: ApplicationModule): Unit = {

    // Since core libraries are using deprecated play.api.libs.ws.WS we need to add wsApi into injector

  }
}