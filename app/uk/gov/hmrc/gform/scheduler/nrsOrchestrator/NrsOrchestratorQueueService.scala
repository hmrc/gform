/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.scheduler.nrsOrchestrator

import org.slf4j.{ Logger, LoggerFactory }
import uk.gov.hmrc.gform.core.FutureSyntax
import uk.gov.hmrc.gform.nrs.NRSConnector
import uk.gov.hmrc.gform.scheduler.{ QueueAlgebra, WorkItemRepo }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.workitem.WorkItem

import scala.concurrent.{ ExecutionContext, Future }

class NrsOrchestratorQueueService(
  nrsConnector: NRSConnector,
  notificationRepository: NrsOrchestratorWorkItemRepo,
  pollerLimit: Int,
  nrsMaxFailureCount: Int,
  nrsRetryIntervalMillis: Long
)(implicit ec: ExecutionContext)
    extends QueueAlgebra[NrsOrchestratorWorkItem] {

  override def sendWorkItem(nrsWorkItem: WorkItem[NrsOrchestratorWorkItem]): Future[Unit] = {
    implicit val hc: HeaderCarrier = HeaderCarrier()
    val workItem = nrsWorkItem.item
    logger.debug(s"Retry of nrsOrchestrator submit envelope id: ${workItem.envelopeId}")

    nrsConnector
      .submit(
        workItem.envelopeId,
        workItem.businessId,
        workItem.notableEvent,
        workItem.onSubmitHeaders,
        workItem.destinationResultData,
        workItem.submissionRef,
        workItem.payload,
        workItem.userAuthToken,
        workItem.identityData,
        workItem.submissionDate
      )
      .flatMap {
        case response if nrsConnector.nrsServerFailure(response) =>
          Future.failed(new RuntimeException(s"NRS server failed $response"))
        case response if response.status != 202 =>
          logger.error(s"Non 202 response from NRS suggests gform failure. NRS Response: $response")
          Future.successful(response)
        case response => Future.successful(response)
      }(ec)
      .void(ec)
  }

  override val repo: WorkItemRepo[NrsOrchestratorWorkItem] = notificationRepository
  override val pollLimit: Int = pollerLimit
  override implicit val executionContext: ExecutionContext = ec
  override val maxFailureCount: Int = nrsMaxFailureCount
  override val retryIntervalMillis: Long = nrsRetryIntervalMillis
  override val logger: Logger = LoggerFactory.getLogger(getClass)
}
