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

package uk.gov.hmrc.gform.scheduler.infoarchive

import org.slf4j.{ Logger, LoggerFactory }
import uk.gov.hmrc.gform.core.FutureSyntax
import uk.gov.hmrc.gform.scheduler.{ QueueAlgebra, WorkItemRepo }
import uk.gov.hmrc.gform.sdes.SdesAlgebra
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesWorkItem
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.workitem.WorkItem

import scala.concurrent.{ ExecutionContext, Future }

class InfoArchiveQueueService(
  infoArchiveAlgebra: SdesAlgebra[Future],
  infoArchiveNotificationRepository: InfoArchiveWorkItemRepo,
  infoArchivePollerLimit: Int,
  infoArchiveMaxFailureCount: Int,
  infoArchiveRetryIntervalMillis: Long
)(implicit ec: ExecutionContext)
    extends QueueAlgebra[SdesWorkItem] {

  override def sendWorkItem(sdesWorkItem: WorkItem[SdesWorkItem]): Future[Unit] = {
    implicit val hc = HeaderCarrier()
    val workItem = sdesWorkItem.item
    logger.debug(s"sending a notification ${workItem.sdesNotifyRequest} in sendWorkItem for info-archive")

    infoArchiveAlgebra
      .notifySDES(
        workItem.correlationId,
        workItem.envelopeId,
        workItem.formTemplateId,
        workItem.submissionRef,
        workItem.sdesNotifyRequest,
        workItem.destination
      )
      .void(ec)
  }

  override val repo: WorkItemRepo[SdesWorkItem] = infoArchiveNotificationRepository
  override val pollLimit: Int = infoArchivePollerLimit
  override implicit val executionContext: ExecutionContext = ec
  override val maxFailureCount: Int = infoArchiveMaxFailureCount
  override val retryIntervalMillis: Long = infoArchiveRetryIntervalMillis
  override val logger: Logger = LoggerFactory.getLogger(getClass)
}
