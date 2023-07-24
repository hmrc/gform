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

package uk.gov.hmrc.gform.scheduler

import org.mongodb.scala.model.IndexModel
import play.api.libs.json.OFormat
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.workitem.ProcessingStatus.PermanentlyFailed
import uk.gov.hmrc.mongo.workitem.{ ProcessingStatus, WorkItem, WorkItemFields, WorkItemRepository }

import java.time.{ Duration, Instant }
import scala.concurrent.{ ExecutionContext, Future }

abstract class WorkItemRepo[N](
  mongoComponent: MongoComponent,
  collectionName: String,
  extraIndexes: Seq[IndexModel],
  now: Instant
)(implicit ec: ExecutionContext, format: OFormat[N])
    extends WorkItemRepository[N](
      collectionName = collectionName,
      mongoComponent = mongoComponent,
      itemFormat = format,
      workItemFields = WorkItemFields.default,
      extraIndexes = extraIndexes
    ) {
  override def now(): Instant = now

  override val inProgressRetryAfter: Duration = Duration.ofMinutes(1)

  def failed(e: WorkItem[N], maxFailureCount: Int): Future[Boolean] =
    if (e.failureCount >= maxFailureCount) permanentlyFailed(e) else markAs(e.id, ProcessingStatus.Failed)

  private def permanentlyFailed(e: WorkItem[N]): Future[Boolean] = complete(e.id, PermanentlyFailed)

  def pullOutstanding(retryIntervalMillis: Long): Future[Option[WorkItem[N]]] =
    super.pullOutstanding(now().minusMillis(retryIntervalMillis), now())
}
