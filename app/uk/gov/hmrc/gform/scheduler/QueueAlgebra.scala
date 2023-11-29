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

import org.slf4j.Logger
import uk.gov.hmrc.mongo.workitem.WorkItem

import scala.concurrent.{ ExecutionContext, Future }

trait QueueAlgebra[Q] {

  val repo: WorkItemRepo[Q]
  val pollLimit: Int
  val maxFailureCount: Int
  val retryIntervalMillis: Long
  val logger: Logger

  implicit val executionContext: ExecutionContext

  def sendWorkItem(workItem: WorkItem[Q]): Future[Unit]

  def processThenMarkAsComplete(acc: Seq[WorkItem[Q]], workItem: WorkItem[Q]): Future[Seq[WorkItem[Q]]] =
    sendWorkItem(workItem).map(_ => repo.completeAndDelete(workItem.id)).map(_ => acc :+ workItem).recoverWith {
      case e =>
        logger.error(s"Failed to send the work-item : $e")
        repo.failed(workItem, maxFailureCount).map(_ => acc)
    }

  def retrieveWorkItems: Future[Seq[WorkItem[Q]]] = {
    def sendNotificationIfFound(count: Int, sentWorkItems: Seq[WorkItem[Q]]): Future[Seq[WorkItem[Q]]] = {
      def retrieveWorkItem(count: Int): Future[Option[WorkItem[Q]]] =
        if (count == pollLimit) Future successful None else repo.pullOutstanding(retryIntervalMillis)

      retrieveWorkItem(count).flatMap {
        case None => Future successful sentWorkItems
        case Some(workItem) =>
          processThenMarkAsComplete(sentWorkItems, workItem).flatMap { workItems =>
            sendNotificationIfFound(count + 1, workItems)
          }
      }
    }
    sendNotificationIfFound(0, Seq.empty)
  }
}
