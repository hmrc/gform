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

package uk.gov.hmrc.gform.scheduler.asynchandlebars

import org.slf4j.{ Logger, LoggerFactory }
import uk.gov.hmrc.gform.scheduler.{ QueueAlgebra, TraceableWorkItem, WorkItemRepo }
import uk.gov.hmrc.gform.submission.handlebars.AsyncHandlebarsApiExecutor
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.workitem.WorkItem

import scala.concurrent.{ ExecutionContext, Future }

class AsyncHandlebarsQueueService(
  asyncHandlebarsExecutor: AsyncHandlebarsApiExecutor[Future],
  asyncHandlebarsRepo: AsyncHandlebarsWorkItemRepo,
  asyncHandlebarsPollerLimit: Int,
  asyncHandlebarsMaxFailureCount: Int,
  asyncHandlebarsRetryIntervalMillis: Long
)(implicit ec: ExecutionContext)
    extends QueueAlgebra[TraceableWorkItem[AsyncHandlebarsWorkItem]] {

  override def sendWorkItem(asyncApiWorkItem: WorkItem[TraceableWorkItem[AsyncHandlebarsWorkItem]]): Future[Unit] = {
    implicit val hc: HeaderCarrier = HeaderCarrier()
    val workItem = asyncApiWorkItem.item
    logger.debug(s"Calling async handlebars API for ${workItem.envelopeId}")

    asyncHandlebarsExecutor.callAPI(workItem)
  }

  override val repo: WorkItemRepo[TraceableWorkItem[AsyncHandlebarsWorkItem]] = asyncHandlebarsRepo
  override val pollLimit: Int = asyncHandlebarsPollerLimit
  override implicit val executionContext: ExecutionContext = ec
  override val maxFailureCount: Int = asyncHandlebarsMaxFailureCount
  override val retryIntervalMillis: Long = asyncHandlebarsRetryIntervalMillis
  override val logger: Logger = LoggerFactory.getLogger(getClass)
}
