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

import org.mongodb.scala.model.{ IndexModel, IndexOptions, Indexes }
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.envelope.EnvelopeModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.objectstore.ObjectStoreModule
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.scheduler.datastore.DataStoreWorkItemRepo
import uk.gov.hmrc.gform.scheduler.dms.DmsWorkItemRepo
import uk.gov.hmrc.gform.sdes.datastore.{ DataStoreCallbackController, DataStoreWorkItemAlgebra, DataStoreWorkItemController, DataStoreWorkItemService }
import uk.gov.hmrc.gform.sdes.dms.{ DmsCallbackController, DmsWorkItemAlgebra, DmsWorkItemController, DmsWorkItemService }
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.sdes._
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.mongo.workitem.{ ProcessingStatus, WorkItem }
import uk.gov.hmrc.objectstore.client.ObjectSummaryWithMd5

import scala.concurrent.{ ExecutionContext, Future }

class SdesModule(
  configModule: ConfigModule,
  wSHttpModule: WSHttpModule,
  mongoModule: MongoModule,
  objectStoreModule: ObjectStoreModule,
  akkaModule: AkkaModule,
  envelopeModule: EnvelopeModule
)(implicit ex: ExecutionContext) {

  private val sdesBaseUrl = configModule.serviceConfig.baseUrl("sdes")
  private val sdesBasePath = configModule.serviceConfig.getString("microservice.services.sdes.base-path")
  private val fileLocationUrl = configModule.serviceConfig.getString("microservice.services.sdes.file-location-url")

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
        )
      ),
      replaceIndexes = true
    )

  private val dmsAuthorizationToken = configModule.serviceConfig.getString("microservice.services.sdes.dms.api-key")
  private val dmsInformationType =
    configModule.serviceConfig.getString("microservice.services.sdes.dms.information-type")
  private val dmsRecipientOrSender =
    configModule.serviceConfig.getString("microservice.services.sdes.dms.recipient-or-sender")

  private val dmsHeaders: Seq[(String, String)] = Seq(
    "x-client-id"  -> dmsAuthorizationToken,
    "Content-Type" -> "application/json"
  )

  val dmsConnector: SdesConnector =
    new SdesConnector(wSHttpModule.auditableWSHttp, sdesBaseUrl, sdesBasePath, dmsHeaders)

  val dmsWorkItemRepo = new DmsWorkItemRepo(mongoModule.mongoComponent)

  val dmsWorkItemService: DmsWorkItemAlgebra[Future] = new DmsWorkItemService(
    dmsWorkItemRepo,
    envelopeModule.envelopeService,
    dmsInformationType,
    dmsRecipientOrSender,
    fileLocationUrl
  )

  val dmsWorkItemController: DmsWorkItemController =
    new DmsWorkItemController(configModule.controllerComponents, dmsWorkItemService)

  private val dataStoreAuthorizationToken =
    configModule.serviceConfig.getString("microservice.services.sdes.data-store.api-key")
  private val dataStoreInformationType =
    configModule.serviceConfig.getString("microservice.services.sdes.data-store.information-type")
  private val dataStoreRecipientOrSender =
    configModule.serviceConfig.getString("microservice.services.sdes.data-store.recipient-or-sender")

  private val dataStoreHeaders: Seq[(String, String)] = Seq(
    "x-client-id"  -> dataStoreAuthorizationToken,
    "Content-Type" -> "application/json"
  )

  val dataStoreConnector: SdesConnector =
    new SdesConnector(wSHttpModule.auditableWSHttp, sdesBaseUrl, sdesBasePath, dataStoreHeaders)

  val dataStoreWorkItemRepo = new DataStoreWorkItemRepo(mongoModule.mongoComponent)

  val dataStoreWorkItemService: DataStoreWorkItemAlgebra[Future] = new DataStoreWorkItemService(
    dataStoreWorkItemRepo,
    envelopeModule.envelopeService,
    dataStoreInformationType,
    dataStoreRecipientOrSender,
    fileLocationUrl
  )

  val dataStoreWorkItemController: DataStoreWorkItemController =
    new DataStoreWorkItemController(configModule.controllerComponents, dataStoreWorkItemService)

  val sdesService: SdesAlgebra[Future] =
    new SdesService(
      dmsConnector,
      dataStoreConnector,
      repoSdesSubmission,
      dmsWorkItemService,
      dataStoreWorkItemService,
      envelopeModule.envelopeService
    )

  val dmsCallbackController: DmsCallbackController =
    new DmsCallbackController(configModule.controllerComponents, sdesService, objectStoreModule.objectStoreService)

  private val dataStoreBasePath =
    configModule.serviceConfig.getString("object-store.data-store.base-path")

  val dataStoreCallbackController: DataStoreCallbackController =
    new DataStoreCallbackController(
      configModule.controllerComponents,
      sdesService,
      objectStoreModule.objectStoreService,
      dataStoreBasePath
    )

  val sdesController: SdesController =
    new SdesController(
      configModule.controllerComponents,
      sdesService,
      objectStoreModule.objectStoreService,
      dataStoreBasePath
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

    override def notifySDES(sdesSubmission: SdesSubmission, objWithSummary: ObjectSummaryWithMd5)(implicit
      hc: HeaderCarrier
    ): FOpt[HttpResponse] =
      fromFutureA(sdesService.notifySDES(sdesSubmission, objWithSummary))

    override def saveSdesSubmission(sdesSubmission: SdesSubmission): FOpt[Unit] =
      fromFutureA(sdesService.saveSdesSubmission(sdesSubmission))

    override def findSdesSubmission(correlationId: CorrelationId): FOpt[Option[SdesSubmission]] =
      fromFutureA(sdesService.findSdesSubmission(correlationId))

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

    override def deleteSdesSubmission(correlation: CorrelationId): FOpt[Unit] =
      fromFutureA(sdesService.deleteSdesSubmission(correlation))

  }

  val foptDataStoreWorkItemService: DataStoreWorkItemAlgebra[FOpt] = new DataStoreWorkItemAlgebra[FOpt] {
    override def pushWorkItem(
      envelopeId: EnvelopeId,
      formTemplateId: FormTemplateId,
      submissionRef: SubmissionRef,
      objWithSummary: ObjectSummaryWithMd5
    ): FOpt[Unit] =
      fromFutureA(dataStoreWorkItemService.pushWorkItem(envelopeId, formTemplateId, submissionRef, objWithSummary))

    override def createNotifyRequest(objSummary: ObjectSummaryWithMd5, correlationId: String): SdesNotifyRequest =
      dataStoreWorkItemService.createNotifyRequest(objSummary, correlationId)

    override def search(
      page: Int,
      pageSize: Int,
      formTemplateId: Option[FormTemplateId],
      status: Option[ProcessingStatus]
    ): FOpt[SdesWorkItemPageData] =
      fromFutureA(dataStoreWorkItemService.search(page, pageSize, formTemplateId, status))

    override def enqueue(id: String): FOpt[Unit] = fromFutureA(dataStoreWorkItemService.enqueue(id))

    override def find(id: String): FOpt[Option[WorkItem[SdesWorkItem]]] = fromFutureA(dataStoreWorkItemService.find(id))

    override def delete(id: String): FOpt[Unit] = fromFutureA(dataStoreWorkItemService.delete(id))
  }
}
