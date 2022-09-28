/*
 * Copyright 2022 HM Revenue & Customs
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
import org.slf4j.LoggerFactory
import org.mongodb.scala.model.{ IndexModel, IndexOptions, Indexes }
import play.api.ApplicationLoader.Context
import play.api._
import play.api.http._
import play.api.i18n.I18nComponents
import play.api.inject.{ Injector, SimpleInjector }
import play.api.mvc.EssentialFilter
import play.api.routing.Router
import play.api.libs.ws.ahc.AhcWSComponents
import uk.gov.hmrc.crypto.CryptoWithKeysFromConfig
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.api.ApiModule
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.formstatistics.FormStatisticsModule
import uk.gov.hmrc.gform.dblookup.DbLookupModule
import uk.gov.hmrc.gform.dms.DmsModule
import uk.gov.hmrc.gform.email.EmailModule
import uk.gov.hmrc.gform.envelope.EnvelopeModule
import uk.gov.hmrc.gform.fileupload.{ FileUploadFrontendAlgebra, FileUploadModule }
import uk.gov.hmrc.gform.form.{ FormModule, FormService }
import uk.gov.hmrc.gform.formmetadata.FormMetadataModule
import uk.gov.hmrc.gform.formtemplate.FormTemplateModule
import uk.gov.hmrc.gform.graphite.GraphiteModule
import uk.gov.hmrc.gform.metrics.MetricsModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.notifier.NotifierModule
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorModule
import uk.gov.hmrc.gform.playcomponents.{ ErrorHandler, PlayComponents, PlayComponentsModule }
import uk.gov.hmrc.gform.proxy.ProxyModule
import uk.gov.hmrc.gform.save4later.FormMongoCache
import uk.gov.hmrc.gform.submission.SubmissionModule
import uk.gov.hmrc.gform.submission.handlebars.HandlebarsHttpApiModule
import uk.gov.hmrc.gform.testonly.TestOnlyModule
import uk.gov.hmrc.gform.time.TimeModule
import uk.gov.hmrc.gform.upscan.UpscanModule
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.gform.obligation.ObligationModule
import uk.gov.hmrc.gform.submission.destinations.DestinationModule
import uk.gov.hmrc.gform.submissionconsolidator.SubmissionConsolidatorModule
import uk.gov.hmrc.mongo.CurrentTimestampSupport
import uk.gov.hmrc.mongo.MongoUtils
import uk.gov.hmrc.mongo.cache.CacheIdType.SimpleCacheId
import uk.gov.hmrc.mongo.cache.MongoCacheRepository
import uk.gov.hmrc.play.bootstrap.config.AppName

import scala.concurrent.duration._
import scala.concurrent.Future
import java.util.concurrent.TimeUnit

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

  private val logger = LoggerFactory.getLogger(getClass)

  val appName = AppName.fromConfiguration(configuration)

  logger.info(s"Starting microservice $appName")

  private val akkaModule = new AkkaModule(materializer, actorSystem)
  protected val playComponents = new PlayComponents(context, self, self)

  protected val configModule = new ConfigModule(configuration, playComponents, controllerComponents, appName)
  private val metricsModule = new MetricsModule(configModule, playComponents, akkaModule, executionContext)
  private val graphiteModule = new GraphiteModule(environment, configuration, applicationLifecycle, metricsModule)
  protected val auditingModule =
    new AuditingModule(configModule, graphiteModule, akkaModule, applicationLifecycle)
  protected val wSHttpModule = new WSHttpModule(auditingModule, configModule, playComponents)
  private val proxyModule = new ProxyModule(configModule)
  private val notifierModule = new NotifierModule(configModule, proxyModule)
  private val emailModule = new EmailModule(configModule, wSHttpModule, notifierModule)
  private val timeModule = new TimeModule
  private val mongoModule = new MongoModule(configModule)
  private val envelopeModule = new EnvelopeModule(mongoModule, configModule)
  val fileUploadModule =
    new FileUploadModule(configModule, wSHttpModule, timeModule, akkaModule, envelopeModule)
  val formTemplateModule = new FormTemplateModule(controllerComponents, mongoModule)
  val pdfGeneratorModule = new PdfGeneratorModule()

  val formMetadaModule = new FormMetadataModule(mongoModule)

  val queryParameterCrypto =
    new CryptoWithKeysFromConfig(baseConfigKey = "upscan.callback.encryption", configModule.typesafeConfig)
  val jsonCrypto = new CryptoWithKeysFromConfig(baseConfigKey = "json.encryption", configModule.typesafeConfig)

  val formMongoCache = new FormMongoCache(
    new MongoCacheRepository[String](
      mongoModule.mongoComponent,
      "forms",
      true,
      configModule.appConfig.formExpiryDays.days,
      new CurrentTimestampSupport(),
      SimpleCacheId
    ) {
      override def ensureIndexes: Future[Seq[String]] = {
        val formExpiry = configModule.appConfig.formExpiryDays.days.toMillis
        val submittedExpiry = configModule.appConfig.submittedFormExpiryHours.hours.toMillis
        val indexes = Seq(
          IndexModel(
            Indexes.ascending("modifiedDetails.lastUpdated"),
            IndexOptions()
              .background(false)
              .name("lastUpdatedIndex")
              .expireAfter(formExpiry, TimeUnit.MILLISECONDS)
          ),
          IndexModel(
            Indexes.ascending("submitDetails.createdAt"),
            IndexOptions()
              .background(false)
              .name("submittedIndex")
              .expireAfter(submittedExpiry, TimeUnit.MILLISECONDS)
          )
        )
        MongoUtils.ensureIndexes(this.collection, indexes, true)
      }
    },
    jsonCrypto,
    timeModule.timeProvider
  )

  val formService: FormService[Future] =
    new FormService(
      formMongoCache,
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
    new DmsModule(
      fileUploadModule,
      pdfGeneratorModule,
      formTemplateModule.formTemplateService,
      configModule.appConfig,
      controllerComponents
    )
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

  val dbLookupModule = new DbLookupModule(controllerComponents, mongoModule)

  val fileUploadFrontendAlgebra: FileUploadFrontendAlgebra[Future] = fileUploadModule.fileUploadFrontendConnector

  val upscanModule = new UpscanModule(
    formService,
    wSHttpModule,
    configModule,
    queryParameterCrypto,
    fileUploadFrontendAlgebra,
    formTemplateModule,
    configModule.appConfig,
    mongoModule
  )

  val formStatisticsModule = new FormStatisticsModule(
    mongoModule,
    formTemplateModule,
    configModule
  )

  override lazy val httpErrorHandler: HttpErrorHandler = new ErrorHandler(
    playComponents.context.environment,
    playComponents.context.initialConfiguration,
    playComponents.context.devContext.map(_.sourceMapper)
  )

  private val apiModule = new ApiModule(configModule, wSHttpModule)

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
    dbLookupModule,
    upscanModule,
    httpErrorHandler,
    formStatisticsModule,
    envelopeModule,
    apiModule
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
    materializer
  )

  logger.info(
    s"Microservice $appName started in mode ${environment.mode} at port ${application.configuration.getOptional[String]("http.port")}"
  )
}

object ApplicationModuleHelper {

  def tweak(applicationModule: ApplicationModule): Unit = {

    // Since core libraries are using deprecated play.api.libs.ws.WS we need to add wsApi into injector

  }
}
