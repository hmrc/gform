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
import uk.gov.hmrc.gform.sdes.alert.{ SdesSubmissionAlertService, SdesWorkItemAlertService }
import uk.gov.hmrc.gform.sdes.renotify.SdesRenotifyQScheduledService
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

import scala.concurrent.{ ExecutionContext, Future }

class SdesModule(
  configModule: ConfigModule,
  wSHttpModule: WSHttpModule,
  mongoModule: MongoModule,
  objectStoreModule: ObjectStoreModule,
  akkaModule: AkkaModule,
  envelopeModule: EnvelopeModule,
  emailModule: EmailModule
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

  private val repoSdesHistory: Repo[SdesHistory] =
    new Repo[SdesHistory](
      "sdesHistory",
      mongoModule.mongoComponent,
      _._id.toString,
      indexes = Seq(
        IndexModel(
          Indexes.ascending("correlationId"),
          IndexOptions()
            .background(false)
            .name("correlationId")
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

  val sdesHistoryService: SdesHistoryService =
    new SdesHistoryService(repoSdesHistory)

  val sdesService: SdesAlgebra[Future] =
    new SdesService(
      dataStoreConnector,
      repoSdesSubmission,
      dmsWorkItemService,
      dataStoreWorkItemService,
      envelopeModule.envelopeService,
      configModule.sdesConfig,
      sdesHistoryService,
      objectStoreModule.objectStoreService
    )(ex, akkaModule.materializer)

  private val lockRepoSdesAlert: MongoLockRepository = new MongoLockRepository(
    mongoModule.mongoComponent,
    new CurrentTimestampSupport()
  )

  val sdesSubmissionAlertService = new SdesSubmissionAlertService(
    configModule.sdesAlertConfig.destination.map(_.map(SdesDestination.fromString)),
    NotifierEmailAddress(configModule.sdesAlertConfig.notifierEmailAddress),
    EmailTemplateId(configModule.sdesAlertConfig.emailTemplateId),
    emailModule.emailLogic,
    repoSdesSubmission,
    lockRepoSdesAlert,
    configModule.sdesAlertConfig.lockDuration
  )

  val sdesWorkItemAlertService = new SdesWorkItemAlertService(
    NotifierEmailAddress(configModule.workItemAlertConfig.notifierEmailAddress),
    EmailTemplateId(configModule.workItemAlertConfig.emailTemplateId),
    emailModule.emailLogic,
    dmsWorkItemRepo,
    dataStoreWorkItemRepo,
    lockRepoSdesAlert,
    configModule.workItemAlertConfig.lockDuration
  )

  private val lockRepoRenotify: MongoLockRepository = new MongoLockRepository(
    mongoModule.mongoComponent,
    new CurrentTimestampSupport()
  )

  val sdesCallbackController: SdesCallbackController =
    new SdesCallbackController(
      configModule.controllerComponents,
      sdesService
    )(ex, akkaModule.actorSystem.scheduler)

  val sdesRenotifyService = new SdesRenotifyService(
    sdesService,
    objectStoreModule.objectStoreService
  )(ex, akkaModule.materializer)

  val sdesController: SdesController =
    new SdesController(
      configModule.controllerComponents,
      sdesService,
      sdesRenotifyService,
      sdesHistoryService
    )(ex)

  val sdesRenotifyQScheduledService = new SdesRenotifyQScheduledService(
    configModule.sdesRenotifyConfig.destinations.map(SdesDestination.fromString),
    sdesRenotifyService,
    sdesService,
    lockRepoRenotify,
    configModule.sdesRenotifyConfig.lockDuration,
    Some(configModule.sdesRenotifyConfig.showBeforeSubmittedAt)
  )

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
      searchKey: Option[String],
      status: Option[NotificationStatus],
      destination: Option[SdesDestination],
      beforeSubmittedAt: Option[Int]
    ): FOpt[SdesSubmissionPageData] =
      fromFutureA(
        sdesService.searchAll(processed, searchKey, status, destination, beforeSubmittedAt)
      )

    override def search(sdesFilter: SdesFilter): FOpt[SdesSubmissionPageData] =
      fromFutureA(
        sdesService.search(sdesFilter)
      )

    override def updateAsManualConfirmed(correlation: CorrelationId): FOpt[Unit] =
      fromFutureA(sdesService.updateAsManualConfirmed(correlation))

    override def getSdesSubmissionsDestination(): FOpt[Seq[SdesSubmissionsStats]] =
      fromFutureA(sdesService.getSdesSubmissionsDestination())

    override def sdesMigration(from: String, to: String): FOpt[UpdateResult] =
      fromFutureA(sdesService.sdesMigration(from, to))

    override def update(notification: CallBackNotification)(implicit hc: HeaderCarrier): FOpt[Unit] =
      fromFutureA(sdesService.update(notification))

    override def resend(correlationId: CorrelationId)(implicit hc: HeaderCarrier): FOpt[Unit] =
      fromFutureA(sdesService.resend(correlationId))
  }

  val foptDmsWorkItemService: DmsWorkItemAlgebra[FOpt] = new DmsWorkItemAlgebra[FOpt] {
    override def pushWorkItem(
      envelopeId: EnvelopeId,
      formTemplateId: FormTemplateId,
      submissionRef: SubmissionRef,
      objWithSummary: ObjectSummaryWithMd5
    ): FOpt[Unit] = fromFutureA(
      dmsWorkItemService.pushWorkItem(envelopeId, formTemplateId, submissionRef, objWithSummary)
    )

    override def createNotifyRequest(
      objSummary: ObjectSummaryWithMd5,
      correlationId: CorrelationId
    ): SdesNotifyRequest =
      dmsWorkItemService.createNotifyRequest(objSummary, correlationId)

    override def search(
      page: Int,
      pageSize: Int,
      formTemplateId: Option[FormTemplateId],
      status: Option[ProcessingStatus]
    ): FOpt[SdesWorkItemPageData] = fromFutureA(dmsWorkItemService.search(page, pageSize, formTemplateId, status))

    override def enqueue(id: String): FOpt[Unit] = fromFutureA(dmsWorkItemService.enqueue(id))

    override def find(id: String): FOpt[Option[WorkItem[SdesWorkItem]]] = fromFutureA(dmsWorkItemService.find(id))

    override def findByEnvelopeId(envelopeId: EnvelopeId): FOpt[List[WorkItem[SdesWorkItem]]] = fromFutureA(
      dmsWorkItemService.findByEnvelopeId(envelopeId)
    )

    override def delete(id: String): FOpt[Unit] = fromFutureA(dmsWorkItemService.delete(id))
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
