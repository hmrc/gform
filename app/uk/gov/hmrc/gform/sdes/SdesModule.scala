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

package uk.gov.hmrc.gform.sdes

import com.mongodb.client.result.UpdateResult
import org.mongodb.scala.model.{ IndexModel, IndexOptions, Indexes }
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.email.EmailModule
import uk.gov.hmrc.gform.envelope.EnvelopeModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.objectstore.ObjectStoreModule
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.scheduler.datastore.DataStoreWorkItemRepo
import uk.gov.hmrc.gform.scheduler.dms.DmsWorkItemRepo
import uk.gov.hmrc.gform.sdes.alert.SdesAlertService
import uk.gov.hmrc.gform.sdes.renotify.SdesReNotifyService
import uk.gov.hmrc.gform.sdes.datastore.{ DataStoreWorkItemAlgebra, DataStoreWorkItemController, DataStoreWorkItemService }
import uk.gov.hmrc.gform.sdes.dms.{ DmsWorkItemAlgebra, DmsWorkItemController, DmsWorkItemService }
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.email.EmailTemplateId
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierEmailAddress
import uk.gov.hmrc.gform.sharedmodel.sdes._
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.mongo.CurrentTimestampSupport
import uk.gov.hmrc.mongo.lock.MongoLockRepository
import uk.gov.hmrc.mongo.workitem.{ ProcessingStatus, WorkItem }
import uk.gov.hmrc.objectstore.client.ObjectSummaryWithMd5

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ ExecutionContext, Future }
import play.api.libs.ws.WSClient

class SdesModule(
  configModule: ConfigModule,
  wSHttpModule: WSHttpModule,
  mongoModule: MongoModule,
  objectStoreModule: ObjectStoreModule,
  akkaModule: AkkaModule,
  envelopeModule: EnvelopeModule,
  emailModule: EmailModule,
  wsClient: WSClient
)(implicit ex: ExecutionContext) {

  private val sdesBaseUrl = configModule.serviceConfig.baseUrl("sdes")
  private val sdesBasePath = configModule.sdesConfig.basePath
  private val fileLocationUrl = configModule.sdesConfig.fileLocationUrl

  private val repoSdesSubmission: Repo[SdesSubmission] =
    new Repo[SdesSubmission](
      "sdesSubmission",
      mongoModule.mongoComponent,
      _._id.value,
      indexes = Seq(
        IndexModel(
          Indexes.ascending("confirmedAt"),
          IndexOptions()
            .background(false)
            .name("confirmedAtIndex")
        ),
        IndexModel(
          Indexes.ascending("isProcessed"),
          IndexOptions()
            .background(false)
            .name("isProcessed")
        )
      )
    )

  val dmsWorkItemRepo = new DmsWorkItemRepo(mongoModule.mongoComponent)

  private val sdesRouting: SdesRouting = configModule.sdesConfig.dms

  val dmsWorkItemService: DmsWorkItemAlgebra[Future] = new DmsWorkItemService(
    dmsWorkItemRepo,
    envelopeModule.envelopeService,
    sdesRouting,
    fileLocationUrl
  )

  val dmsWorkItemController: DmsWorkItemController =
    new DmsWorkItemController(configModule.controllerComponents, dmsWorkItemService)

  val dataStoreConnector: SdesConnector =
    new SdesConnector(wSHttpModule.auditableWSHttp, sdesBaseUrl, sdesBasePath)

  val dataStoreWorkItemRepo = new DataStoreWorkItemRepo(mongoModule.mongoComponent)

  private val dataStoreWorkItemService: DataStoreWorkItemAlgebra[Future] = new DataStoreWorkItemService(
    dataStoreWorkItemRepo,
    envelopeModule.envelopeService,
    fileLocationUrl
  )

  val dataStoreWorkItemController: DataStoreWorkItemController =
    new DataStoreWorkItemController(configModule.controllerComponents, dataStoreWorkItemService)

  val sdesService: SdesAlgebra[Future] =
    new SdesService(
      dataStoreConnector,
      repoSdesSubmission,
      dmsWorkItemService,
      dataStoreWorkItemService,
      envelopeModule.envelopeService,
      configModule.sdesConfig
    )

  private val alertSdesDestination: Option[Seq[String]] =
    configModule.configuration.getOptional[Seq[String]]("alert.sdes.destination")
  private val alertSdesNotifierEmailAddress: String =
    configModule.typesafeConfig.getString("alert.sdes.notifierEmailAddress")
  private val alertSdesEmailTemplateId: String = configModule.typesafeConfig.getString("alert.sdes.emailTemplateId")
  private val alertSdesMongodbLockTimeoutDuration: FiniteDuration =
    FiniteDuration(configModule.typesafeConfig.getDuration("alert.sdes.lockDuration").toNanos, TimeUnit.NANOSECONDS)
  private val lockRepoSdesAlert: MongoLockRepository = new MongoLockRepository(
    mongoModule.mongoComponent,
    new CurrentTimestampSupport()
  )

  val sdesAlertService = new SdesAlertService(
    alertSdesDestination.map(_.map(SdesDestination.fromString)),
    NotifierEmailAddress(alertSdesNotifierEmailAddress),
    EmailTemplateId(alertSdesEmailTemplateId),
    emailModule.emailLogic,
    repoSdesSubmission,
    lockRepoSdesAlert,
    alertSdesMongodbLockTimeoutDuration
  )

  private val renotifyDestinations: Seq[String] =
    configModule.configuration
      .getOptional[Seq[String]]("renotify.sdes.destinations")
      .getOrElse(Seq.empty)

  private val reNotifyMongodbLockTimeoutDuration: FiniteDuration =
    FiniteDuration(
      configModule.typesafeConfig.getDuration("renotify.sdes.lockDuration").toNanos,
      TimeUnit.NANOSECONDS
    )

  private val showBeforeLastUpdatedAt: Option[Int] =
    configModule.configuration
      .getOptional[Int]("renotify.sdes.showBeforeLastUpdatedAt")

  private val lockRepoReNotify: MongoLockRepository = new MongoLockRepository(
    mongoModule.mongoComponent,
    new CurrentTimestampSupport()
  )

  private val gformBaseUrl: String = configModule.serviceConfig.baseUrl("gform")

  val reNotifyService = new SdesReNotifyService(
    renotifyDestinations.map(SdesDestination.fromString),
    wsClient,
    sdesService,
    lockRepoReNotify,
    reNotifyMongodbLockTimeoutDuration,
    showBeforeLastUpdatedAt,
    gformBaseUrl
  )

  val sdesCallbackController: SdesCallbackController =
    new SdesCallbackController(
      configModule.controllerComponents,
      sdesService,
      objectStoreModule.objectStoreService
    )

  val sdesController: SdesController =
    new SdesController(
      configModule.controllerComponents,
      sdesService,
      objectStoreModule.objectStoreService
    )(ex, akkaModule.materializer)

  val foptSdesService: SdesAlgebra[FOpt] = new SdesAlgebra[FOpt] {

    override def notifySDES(
      correlationId: CorrelationId,
      envelopeId: EnvelopeId,
      formTemplateId: FormTemplateId,
      submissionRef: SubmissionRef,
      notifyRequest: SdesNotifyRequest,
      destination: SdesDestination
    )(implicit
      hc: HeaderCarrier
    ): FOpt[HttpResponse] =
      fromFutureA(
        sdesService.notifySDES(correlationId, envelopeId, formTemplateId, submissionRef, notifyRequest, destination)
      )

    override def renotifySDES(sdesSubmission: SdesSubmission, objWithSummary: ObjectSummaryWithMd5)(implicit
      hc: HeaderCarrier
    ): FOpt[HttpResponse] =
      fromFutureA(sdesService.renotifySDES(sdesSubmission, objWithSummary))

    override def saveSdesSubmission(sdesSubmission: SdesSubmission): FOpt[Unit] =
      fromFutureA(sdesService.saveSdesSubmission(sdesSubmission))

    override def findSdesSubmission(correlationId: CorrelationId): FOpt[Option[SdesSubmission]] =
      fromFutureA(sdesService.findSdesSubmission(correlationId))

    override def findSdesSubmissionByEnvelopeId(envelopeId: EnvelopeId): FOpt[List[SdesSubmission]] =
      fromFutureA(sdesService.findSdesSubmissionByEnvelopeId(envelopeId))

    override def searchAll(
      processed: Option[Boolean],
      formTemplateId: Option[FormTemplateId],
      status: Option[NotificationStatus],
      showBeforeAt: Option[Boolean],
      destination: Option[SdesDestination],
      showBeforeLastUpdatedAt: Option[Int]
    ): FOpt[SdesSubmissionPageData] =
      fromFutureA(
        sdesService.searchAll(processed, formTemplateId, status, showBeforeAt, destination, showBeforeLastUpdatedAt)
      )

    override def search(
      page: Int,
      pageSize: Int,
      processed: Option[Boolean],
      formTemplateId: Option[FormTemplateId],
      status: Option[NotificationStatus],
      showBeforeAt: Option[Boolean],
      destination: Option[SdesDestination]
    ): FOpt[SdesSubmissionPageData] =
      fromFutureA(sdesService.search(page, pageSize, processed, formTemplateId, status, showBeforeAt, destination))

    override def updateAsManualConfirmed(correlation: CorrelationId): FOpt[Unit] =
      fromFutureA(sdesService.updateAsManualConfirmed(correlation))

    override def getSdesSubmissionsDestination(): FOpt[Seq[SdesSubmissionsStats]] =
      fromFutureA(sdesService.getSdesSubmissionsDestination())

    override def sdesMigration(from: String, to: String): FOpt[UpdateResult] =
      fromFutureA(sdesService.sdesMigration(from, to))

  }

  val foptDataStoreWorkItemService: DataStoreWorkItemAlgebra[FOpt] = new DataStoreWorkItemAlgebra[FOpt] {
    override def pushWorkItem(
      envelopeId: EnvelopeId,
      formTemplateId: FormTemplateId,
      submissionRef: SubmissionRef,
      objWithSummary: ObjectSummaryWithMd5,
      dataStoreRouting: SdesRouting,
      destination: SdesDestination
    ): FOpt[Unit] =
      fromFutureA(
        dataStoreWorkItemService
          .pushWorkItem(envelopeId, formTemplateId, submissionRef, objWithSummary, dataStoreRouting, destination)
      )

    override def createNotifyRequest(
      objSummary: ObjectSummaryWithMd5,
      correlationId: String,
      dataStoreRouting: SdesRouting
    ): SdesNotifyRequest =
      dataStoreWorkItemService.createNotifyRequest(objSummary, correlationId, dataStoreRouting)

    override def searchAll(
      formTemplateId: Option[FormTemplateId],
      status: Option[ProcessingStatus]
    ): FOpt[SdesWorkItemPageData] =
      fromFutureA(dataStoreWorkItemService.searchAll(formTemplateId, status))

    override def search(
      page: Int,
      pageSize: Int,
      formTemplateId: Option[FormTemplateId],
      status: Option[ProcessingStatus]
    ): FOpt[SdesWorkItemPageData] =
      fromFutureA(dataStoreWorkItemService.search(page, pageSize, formTemplateId, status))

    override def enqueue(id: String): FOpt[Unit] = fromFutureA(dataStoreWorkItemService.enqueue(id))

    override def find(id: String): FOpt[Option[WorkItem[SdesWorkItem]]] = fromFutureA(dataStoreWorkItemService.find(id))

    override def findByEnvelopeId(envelopeId: EnvelopeId): FOpt[List[WorkItem[SdesWorkItem]]] = fromFutureA(
      dataStoreWorkItemService.findByEnvelopeId(envelopeId)
    )

    override def delete(id: String): FOpt[Unit] = fromFutureA(dataStoreWorkItemService.delete(id))
  }
}
