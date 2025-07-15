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

package uk.gov.hmrc.gform

import cats.instances.future._
import org.mongodb.scala.model.Indexes.{ ascending, compoundIndex }
import org.mongodb.scala.model.{ IndexModel, IndexOptions }
import org.slf4j.LoggerFactory
import play.api.ApplicationLoader.Context
import play.api._
import play.api.http._
import play.api.i18n.I18nComponents
import play.api.inject.Injector
import play.api.inject.SimpleInjector
import play.api.libs.ws.ahc.{ AhcWSClient, AhcWSClientConfigFactory, AhcWSComponents }
import play.api.mvc.EssentialFilter
import play.api.routing.Router
import uk.gov.hmrc.crypto.{ Decrypter, Encrypter, SymmetricCryptoFactory }
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.builder.BuilderModule
import uk.gov.hmrc.gform.companieshouse.CompaniesHouseModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.dblookup.DbLookupModule
import uk.gov.hmrc.gform.des.DesModule
import uk.gov.hmrc.gform.dms.DmsModule
import uk.gov.hmrc.gform.email.EmailModule
import uk.gov.hmrc.gform.employments.EmploymentsModule
import uk.gov.hmrc.gform.envelope.EnvelopeModule
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.form.FormModule
import uk.gov.hmrc.gform.form.FormService
import uk.gov.hmrc.gform.formmetadata.FormMetadataModule
import uk.gov.hmrc.gform.formstatistics.FormStatisticsModule
import uk.gov.hmrc.gform.formtemplate.FormTemplateModule
import uk.gov.hmrc.gform.gformfrontend.GformFrontendModule
import uk.gov.hmrc.gform.graphite.GraphiteModule
import uk.gov.hmrc.gform.handlebarstemplate.{ HandlebarsSchemaAlgebra, HandlebarsSchemaService, HandlebarsTemplateAlgebra, HandlebarsTemplateModule, HandlebarsTemplateService }
import uk.gov.hmrc.gform.history.HistoryModule
import uk.gov.hmrc.gform.metrics.MetricsModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.notificationbanner.NotificationBannerModule
import uk.gov.hmrc.gform.notifier.NotifierModule
import uk.gov.hmrc.gform.objectstore.ObjectStoreModule
import uk.gov.hmrc.gform.obligation.ObligationModule
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorModule
import uk.gov.hmrc.gform.playcomponents.ErrorHandler
import uk.gov.hmrc.gform.playcomponents.PlayComponents
import uk.gov.hmrc.gform.playcomponents.PlayComponentsModule
import uk.gov.hmrc.gform.proxy.ProxyModule
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.retrieval.AuthRetrievalModule
import uk.gov.hmrc.gform.save4later.FormMongoCache
import uk.gov.hmrc.gform.scheduler.SchedulerModule
import uk.gov.hmrc.gform.sdes.SdesModule
import uk.gov.hmrc.gform.sharedmodel.{ HandlebarsSchema, HandlebarsTemplate }
import uk.gov.hmrc.gform.shutter.ShutterModule
import uk.gov.hmrc.gform.submission.SubmissionModule
import uk.gov.hmrc.gform.submission.destinations.DestinationModule
import uk.gov.hmrc.gform.submission.handlebars.HandlebarsHttpApiModule
import uk.gov.hmrc.gform.submissionconsolidator.SubmissionConsolidatorModule
import uk.gov.hmrc.gform.testonly.TestOnlyModule
import uk.gov.hmrc.gform.testonly.TestOnlyFormService
import uk.gov.hmrc.gform.testonly.SnapshotMongoCache
import uk.gov.hmrc.gform.time.TimeModule
import uk.gov.hmrc.gform.translation.TranslationModule
import uk.gov.hmrc.gform.upscan.UpscanModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.http.client.{ HttpClientV2, HttpClientV2Impl }
import uk.gov.hmrc.mongo.CurrentTimestampSupport
import uk.gov.hmrc.mongo.MongoUtils
import uk.gov.hmrc.mongo.cache.CacheIdType.SimpleCacheId
import uk.gov.hmrc.mongo.cache.MongoCacheRepository
import uk.gov.hmrc.play.bootstrap.LoggerModule
import uk.gov.hmrc.play.bootstrap.config.AppName

import java.util.concurrent.TimeUnit
import scala.concurrent.{ Future, Promise }
import scala.concurrent.duration._

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

  private val loggerModule = new LoggerModule
  loggerModule.bindings(environment, configuration)
  private val logger = LoggerFactory.getLogger(getClass)

  private val appName = AppName.fromConfiguration(configuration)

  logger.info(s"Starting microservice $appName")

  private val httpClientV2: HttpClientV2 =
    new HttpClientV2Impl(
      wsClient = AhcWSClient(AhcWSClientConfigFactory.forConfig(configuration.underlying)),
      actorSystem,
      configuration,
      hooks = Seq.empty
    )

  private val akkaModule = new AkkaModule(materializer, actorSystem)
  protected val playComponents = new PlayComponents(context, self, self)

  protected val configModule = new ConfigModule(configuration, controllerComponents)
  private val metricsModule = new MetricsModule(playComponents, akkaModule, executionContext)
  private val graphiteModule = new GraphiteModule(configuration, applicationLifecycle, metricsModule)
  protected val auditingModule =
    new AuditingModule(configModule, graphiteModule, akkaModule, applicationLifecycle)
  protected lazy val wSHttpModule = new WSHttpModule(httpClientV2)
  private val proxyModule = new ProxyModule(configModule)
  private val notifierModule = new NotifierModule(configModule, proxyModule)
  private val timeModule = new TimeModule
  private val mongoModule = new MongoModule(configModule)
  private val envelopeModule = new EnvelopeModule(mongoModule, configModule)
  private val objectStoreModule =
    new ObjectStoreModule(configModule, wsClient, akkaModule, envelopeModule, wSHttpModule)

  private val shutterModule = new ShutterModule(mongoModule, configModule)
  private val notificationBannerModule = new NotificationBannerModule(mongoModule, configModule)
  private val handlebarsTemplateRepo: Repo[HandlebarsTemplate] =
    new Repo[HandlebarsTemplate]("handlebarsTemplate", mongoModule.mongoComponent, _._id.value)
  private val handlebarsTemplateService: HandlebarsTemplateAlgebra[Future] = new HandlebarsTemplateService(
    handlebarsTemplateRepo
  )
  private val handlebarsSchemaRepo: Repo[HandlebarsSchema] =
    new Repo[HandlebarsSchema]("handlebarsSchema", mongoModule.mongoComponent, _._id.value)
  private val handlebarsSchemaService: HandlebarsSchemaAlgebra[Future] = new HandlebarsSchemaService(
    handlebarsSchemaRepo
  )
  private val historyModule = new HistoryModule(configModule, mongoModule)

  private val gformFrontendModule = new GformFrontendModule(wSHttpModule, configModule)

  val formTemplateModule =
    new FormTemplateModule(
      controllerComponents,
      mongoModule,
      shutterModule,
      notificationBannerModule,
      handlebarsTemplateService,
      handlebarsSchemaService,
      historyModule,
      configModule,
      gformFrontendModule
    )

  private val handlebarsPayloadModule =
    new HandlebarsTemplateModule(
      controllerComponents,
      handlebarsTemplateService,
      formTemplateModule,
      handlebarsSchemaService
    )

  private val emailModule = new EmailModule(configModule, wSHttpModule, notifierModule, formTemplateModule)
  private val translationModule =
    new TranslationModule(
      formTemplateModule,
      historyModule,
      configModule,
      mongoModule,
      gformFrontendModule.gformFrontendConnector
    )
  private val pdfGeneratorModule = new PdfGeneratorModule(playComponents.context.environment)

  private val sdesModule =
    new SdesModule(
      configModule,
      mongoModule,
      objectStoreModule,
      akkaModule,
      envelopeModule,
      emailModule
    )
  private val fileUploadModule =
    new FileUploadModule(
      configModule,
      wSHttpModule,
      timeModule,
      akkaModule,
      envelopeModule,
      objectStoreModule,
      sdesModule
    )

  private val formMetadaModule = new FormMetadataModule(mongoModule)

  private val queryParameterCrypto = SymmetricCryptoFactory.aesCryptoFromConfig(
    baseConfigKey = "upscan.callback.encryption",
    configModule.typesafeConfig
  )

  private val jsonCrypto: Encrypter with Decrypter =
    SymmetricCryptoFactory.aesCryptoFromConfig(baseConfigKey = "json.encryption", configModule.typesafeConfig)

  private val prodExpiryDays: Int = configModule.appConfig.formExpiryDays
  private val prodCreatedExpiryDays: Int = configModule.appConfig.formExpiryDaysFromCreation
  private val prodSubmittedExpiryHours: Int = configModule.appConfig.submittedFormExpiryHours
  private val formsCacheRepository =
    createMongoCacheRepository("forms", prodExpiryDays, prodCreatedExpiryDays, prodSubmittedExpiryHours)
  private val formMongoCache = new FormMongoCache(
    formsCacheRepository,
    jsonCrypto,
    timeModule.timeProvider
  )

  private def createMongoCacheRepository(
    collectionName: String,
    expiryDays: Int,
    createdExpiryDays: Int,
    submittedExpiryHours: Int
  ) = new MongoCacheRepository[String](
    mongoModule.mongoComponent,
    collectionName,
    true,
    expiryDays.days,
    new CurrentTimestampSupport(),
    SimpleCacheId
  ) {
    override def ensureIndexes(): Future[Seq[String]] = {
      val formExpiry = expiryDays.days.toMillis
      val createdFormExpiry = createdExpiryDays.days.toMillis
      val submittedExpiry = submittedExpiryHours.hours.toMillis
      val indexes = Seq(
        IndexModel(
          ascending("modifiedDetails.createdAt"),
          IndexOptions()
            .background(false)
            .name("createdAtIndex")
            .expireAfter(createdFormExpiry, TimeUnit.MILLISECONDS)
        ),
        IndexModel(
          ascending("modifiedDetails.lastUpdated"),
          IndexOptions()
            .background(false)
            .name("lastUpdatedIndex")
            .expireAfter(formExpiry, TimeUnit.MILLISECONDS)
        ),
        IndexModel(
          ascending("submitDetails.createdAt"),
          IndexOptions()
            .background(false)
            .name("submittedIndex")
            .expireAfter(submittedExpiry, TimeUnit.MILLISECONDS)
        ),
        IndexModel(
          ascending("data.form.formTemplateId"),
          IndexOptions()
            .background(false)
            .name("formTemplateIdIdx")
        ),
        IndexModel(
          compoundIndex(ascending("data.form.status"), ascending("modifiedDetails.lastUpdated")),
          IndexOptions()
            .background(false)
            .name("statusLastUpdatedIdx")
        )
      )
      MongoUtils.ensureIndexes(this.collection, indexes, true)
    }
  }

  private val formService: FormService[Future] = createFormService(formMongoCache)
  private def createFormService(cache: FormMongoCache): FormService[Future] =
    new FormService(
      cache,
      objectStoreModule.objectStoreService,
      formTemplateModule.formTemplateService,
      formMetadaModule.formMetadataService,
      formTemplateModule.formRedirectService,
      configModule.appConfig.formExpiryDays,
      configModule.appConfig.formExpiryDaysFromCreation
    )

  private val formModule =
    new FormModule(configModule, objectStoreModule.objectStoreService, formService)

  private val destinationModule =
    new DestinationModule(configModule, mongoModule, formModule, formMetadaModule, objectStoreModule)

  private val validationModule = new DesModule(wSHttpModule, configModule)

  private val handlebarsModule = new HandlebarsHttpApiModule(wSHttpModule, configModule)

  private val submissionConsolidatorModule = new SubmissionConsolidatorModule(wSHttpModule, formModule, configModule)

  private val submissionModule =
    new SubmissionModule(
      configModule,
      mongoModule,
      pdfGeneratorModule,
      formModule,
      formTemplateModule,
      timeModule,
      emailModule,
      submissionConsolidatorModule,
      handlebarsModule,
      destinationModule,
      notifierModule,
      envelopeModule,
      objectStoreModule,
      sdesModule,
      materializer
    )

  private val retrievalModule =
    new AuthRetrievalModule(
      mongoModule,
      configModule,
      jsonCrypto,
      formService,
      formMongoCache,
      formMetadaModule.formMetadataService
    )

  private val dmsModule =
    new DmsModule(
      fileUploadModule,
      pdfGeneratorModule,
      configModule,
      controllerComponents
    )
  private val obligationModule = new ObligationModule(wSHttpModule, configModule)
  private val employmentsModule = new EmploymentsModule(wSHttpModule, configModule)

  private val snapshotsMongoCache = createMongoCacheRepository(
    "snapshots",
    configModule.snapshotExpiryDays,
    configModule.snapshotCreatedExpiryDays,
    configModule.snapshotSubmittedExpiryHours
  )
  private val snapshotMongoCache = new SnapshotMongoCache(
    snapshotsMongoCache
  )
  private val testOnlyFormService = new TestOnlyFormService(
    snapshotMongoCache,
    formMongoCache,
    formTemplateModule.formTemplateService,
    formTemplateModule.handler,
    envelopeModule.foptEnvelopeService,
    objectStoreModule.foptObjectStoreService,
    formMetadaModule.formMetadataService
  )
  private val testOnlyModule =
    new TestOnlyModule(
      mongoModule,
      wSHttpModule,
      configModule,
      playComponents,
      formService,
      testOnlyFormService,
      formTemplateModule,
      destinationModule,
      controllerComponents,
      submissionModule
    )

  val dbLookupModule = new DbLookupModule(controllerComponents, mongoModule)

  val upscanModule = new UpscanModule(
    formService,
    configModule,
    queryParameterCrypto,
    formTemplateModule,
    configModule.appConfig,
    mongoModule,
    objectStoreModule,
    auditingModule
  )

  private val formStatisticsModule = new FormStatisticsModule(
    formsCacheRepository,
    formTemplateModule,
    configModule
  )

  override lazy val httpErrorHandler: HttpErrorHandler = new ErrorHandler(
    playComponents.context.environment,
    playComponents.context.initialConfiguration,
    playComponents.context.devContext.map(_.sourceMapper)
  )

  new SchedulerModule(configModule, mongoModule, sdesModule, akkaModule, applicationLifecycle)

  private val builderModule =
    new BuilderModule(
      controllerComponents,
      formTemplateModule.formTemplateService,
      historyModule,
      gformFrontendModule.gformFrontendConnector
    )

  val companiesHouseModule = new CompaniesHouseModule(
    configModule,
    auditingModule,
    wSHttpModule
  )

  private val playComponentsModule = new PlayComponentsModule(
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
    employmentsModule,
    emailModule,
    dbLookupModule,
    upscanModule,
    httpErrorHandler,
    formStatisticsModule,
    envelopeModule,
    translationModule,
    objectStoreModule,
    sdesModule,
    notificationBannerModule,
    builderModule,
    shutterModule,
    handlebarsPayloadModule,
    historyModule,
    retrievalModule,
    companiesHouseModule
  )

  override lazy val httpRequestHandler: HttpRequestHandler = playComponentsModule.httpRequestHandler
  override lazy val httpFilters: Seq[EssentialFilter] = playComponentsModule.httpFilters

  override def router: Router = playComponentsModule.router

  private val customInjector: Injector = new SimpleInjector(injector) + wsClient
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

  actorSystem.scheduler.scheduleOnce(
    scala.concurrent.duration.Duration(5, "seconds")
  ) {
    if (!configModule.isProd) {
      val promise = Promise[Unit]()
      testOnlyModule.testOnlyController.reloadTemplates().onComplete {
        case scala.util.Success(_)  => promise.success(())
        case scala.util.Failure(ex) => promise.failure(ex)
      }
    }
  }

  logger.info(
    s"Microservice $appName started in mode ${environment.mode} at port ${application.configuration.getOptional[String]("http.port")}"
  )
}
