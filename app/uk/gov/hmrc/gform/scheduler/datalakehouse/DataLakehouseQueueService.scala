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

package uk.gov.hmrc.gform.scheduler.datalakehouse

import org.slf4j.{ Logger, LoggerFactory }
import uk.gov.hmrc.gform.core.FutureSyntax
import uk.gov.hmrc.gform.scheduler.{ QueueAlgebra, WorkItemRepo }
import uk.gov.hmrc.gform.sdes.SdesAlgebra
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesWorkItem
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.workitem.WorkItem

import scala.concurrent.{ ExecutionContext, Future }

class DataLakehouseQueueService(
  dataLakehouseAlgebra: SdesAlgebra[Future],
  dataLakehouseNotificationRepository: DataLakehouseWorkItemRepo,
  dataLakehousePollerLimit: Int,
  dataLakehouseMaxFailureCount: Int,
  dataLakehouseRetryIntervalMillis: Long
)(implicit ec: ExecutionContext)
    extends QueueAlgebra[SdesWorkItem] {

  override def sendWorkItem(sdesWorkItem: WorkItem[SdesWorkItem]): Future[Unit] = {
    implicit val hc = HeaderCarrier()
    val workItem = sdesWorkItem.item
    logger.debug(s"sending a notification for ${workItem.envelopeId} in sendWorkItem to data-store")

    dataLakehouseAlgebra
      .notifySDES(
        workItem.correlationId,
        workItem.envelopeId,
        workItem.formTemplateId,
        workItem.submissionRef,
        workItem.destination,
        workItem.filePrefix,
        workItem.submissionPrefix
      )
      .void(ec)
  }

  override val repo: WorkItemRepo[SdesWorkItem] = dataLakehouseNotificationRepository
  override val pollLimit: Int = dataLakehousePollerLimit
  override implicit val executionContext: ExecutionContext = ec
  override val maxFailureCount: Int = dataLakehouseMaxFailureCount
  override val retryIntervalMillis: Long = dataLakehouseRetryIntervalMillis
  override val logger: Logger = LoggerFactory.getLogger(getClass)
}
