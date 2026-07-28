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
import uk.gov.hmrc.gform.nrs.NRSConnector
import uk.gov.hmrc.gform.scheduler.{ QueueAlgebra, TraceableWorkItem, WorkItemRepo }
import uk.gov.hmrc.gform.submission.{ WorkItemHistory, WorkItemHistoryAlgebra }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.workitem.WorkItem

import scala.concurrent.{ ExecutionContext, Future }

class NrsOrchestratorQueueService(
  nrsConnector: NRSConnector,
  notificationRepository: NrsOrchestratorWorkItemRepo,
  workItemHistoryService: WorkItemHistoryAlgebra[Future],
  pollerLimit: Int,
  nrsMaxFailureCount: Int,
  nrsRetryIntervalMillis: Long
)(ec: ExecutionContext)
    extends QueueAlgebra[TraceableWorkItem[NrsOrchestratorWorkItemData]] {

  override def sendWorkItem(nrsWorkItem: WorkItem[TraceableWorkItem[NrsOrchestratorWorkItemData]]): Future[Unit] = {
    implicit val hc: HeaderCarrier = HeaderCarrier()
    val workItem = nrsWorkItem.item
    logger.debug(s"Retry of nrsOrchestrator submit envelope id: ${workItem.envelopeId}")

    for {
      response <- nrsConnector
                    .submit(
                      workItem.envelopeId,
                      workItem.data.businessId,
                      workItem.data.notableEvent,
                      workItem.data.onSubmitHeaders,
                      workItem.data.destinationResultData,
                      workItem.submissionRef,
                      workItem.data.payload,
                      workItem.data.userAuthToken,
                      workItem.data.identityData,
                      workItem.data.submissionDate,
                      workItem.formTemplateId,
                      workItem.destinationId
                    )
      history = WorkItemHistory.create(
                  workItem.envelopeId,
                  workItem.formTemplateId,
                  workItem.submissionRef,
                  workItem.destinationId,
                  response.status,
                  response.body
                )
      _ <- workItemHistoryService.save(history)
      _ <-
        if (nrsConnector.nrsServerFailure(response)) {
          Future.failed(
            new RuntimeException(
              s"""NRS Orchestrator work-item failed due to NRS server failure.
                 |NRS Response Status: ${response.status}
                 |NRS Response Body: ${response.body}
                 |Envelope id: ${workItem.envelopeId}
                 |WorkItem Id: ${nrsWorkItem.id}""".stripMargin
            )
          )
        } else if (response.status != 202) {
          Future
            .failed( //TODO: Work item should not retry in cases where gform fails. Future.failed response will trigger a retry of work-item.
              new RuntimeException(
                s"""NRS Orchestrator work-item failed.
                   | Non 202, 422, 5xx response from NRS suggest gform failure.
                   | NRS Response status: ${response.status}.
                   | NRS response body: ${response.body}.
                   | Envelope id: ${workItem.envelopeId}
                   | WorkItem Id: ${nrsWorkItem.id}""".stripMargin
              )
            )
        } else {
          Future.unit
        }
    } yield ()
  }

  override val repo: WorkItemRepo[TraceableWorkItem[NrsOrchestratorWorkItemData]] = notificationRepository
  override val pollLimit: Int = pollerLimit
  override implicit val executionContext: ExecutionContext = ec
  override val maxFailureCount: Int = nrsMaxFailureCount
  override val retryIntervalMillis: Long = nrsRetryIntervalMillis
  override val logger: Logger = LoggerFactory.getLogger(getClass)
}
