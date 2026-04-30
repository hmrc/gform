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

package uk.gov.hmrc.gform.submission

import cats.implicits.toFunctorOps
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Filters.equal
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.history.{ WorkItemHistoryData, WorkItemHistoryPageData }

import scala.concurrent.{ ExecutionContext, Future }

trait WorkItemHistoryAlgebra[F[_]] {
  def save(workItemHistory: WorkItemHistory): F[Unit]
  def search(
    page: Int,
    pageSize: Int,
    envelopeId: Option[EnvelopeId],
    formTemplateId: Option[FormTemplateId]
  ): F[WorkItemHistoryPageData]

}

class WorkItemHistoryService(repo: Repo[WorkItemHistory])(implicit ec: ExecutionContext)
    extends WorkItemHistoryAlgebra[Future] {
  private val logger = LoggerFactory.getLogger(getClass)
  override def save(workItemHistory: WorkItemHistory): Future[Unit] =
    repo
      .upsert(workItemHistory)
      .toFuture
      .as(logger.info(s"ApiHistoryService.save(${workItemHistory.envelopeId.value}) - upserting"))

  override def search(
    page: Int,
    pageSize: Int,
    envelopeId: Option[EnvelopeId],
    formTemplateId: Option[FormTemplateId]
  ): Future[WorkItemHistoryPageData] = {
    val sort: Bson = equal("createdAt", -1)

    val skip: Int = page * pageSize

    val queryByEnvelopeId = envelopeId.fold(Filters.empty())(e => Filters.equal("envelopeId", e.value))

    val query = formTemplateId.fold(queryByEnvelopeId)(f =>
      Filters.and(queryByEnvelopeId, Filters.equal("formTemplateId", f.value))
    )

    for {
      workItems <- repo.collection
                     .find(query)
                     .sort(sort)
                     .skip(skip)
                     .limit(pageSize)
                     .toFuture()
                     .map(_.toList)
      workItemData = workItems.map(item => WorkItemHistoryData.fromWorkItemHistory(item))
      count <- repo.collection.countDocuments(query).toFuture()
    } yield WorkItemHistoryPageData(workItemData, count)
  }
}
