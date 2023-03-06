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

import cats.syntax.functor._
import org.bson.types.ObjectId
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Filters.equal
import uk.gov.hmrc.gform.scheduler.sdes.SdesWorkItemRepo
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.sdes.{ SdesWorkItem, SdesWorkItemData, SdesWorkItemPageData }
import uk.gov.hmrc.mongo.workitem.{ ProcessingStatus, WorkItem }

import scala.concurrent.{ ExecutionContext, Future }

trait SdesWorkItemAlgebra[F[_]] {

  def search(
    page: Int,
    pageSize: Int,
    formTemplateId: Option[FormTemplateId],
    status: Option[ProcessingStatus]
  ): F[SdesWorkItemPageData]

  def enqueue(id: String): F[Unit]

  def find(id: String): F[Option[WorkItem[SdesWorkItem]]]

  def delete(id: String): F[Unit]
}

class SdesWorkItemService(sdesWorkItemRepo: SdesWorkItemRepo)(implicit ec: ExecutionContext)
    extends SdesWorkItemAlgebra[Future] {

  override def search(
    page: Int,
    pageSize: Int,
    formTemplateId: Option[FormTemplateId],
    status: Option[ProcessingStatus]
  ): Future[SdesWorkItemPageData] = {
    val queryByFormTemplateId = formTemplateId.fold(Filters.exists("_id"))(t => equal("item.formTemplateId", t.value))
    val query = status.fold(queryByFormTemplateId)(s => Filters.and(equal("status", s.name), queryByFormTemplateId))
    val sort = equal("receivedAt", -1)

    val skip = page * pageSize
    for {
      sdesWorkItem <-
        sdesWorkItemRepo.collection.find(query).sort(sort).skip(skip).limit(pageSize).toFuture().map(_.toList)
      count <- sdesWorkItemRepo.collection.countDocuments(query).toFuture()
    } yield SdesWorkItemPageData(sdesWorkItem.map(SdesWorkItemData.fromWorkItem), count)
  }

  override def delete(id: String): Future[Unit] =
    sdesWorkItemRepo.collection.deleteOne(equal("_id", new ObjectId(id))).toFuture().map(_.getDeletedCount > 0).void

  override def enqueue(id: String): Future[Unit] =
    sdesWorkItemRepo.markAs(new ObjectId(id), ProcessingStatus.ToDo).void

  override def find(id: String): Future[Option[WorkItem[SdesWorkItem]]] =
    sdesWorkItemRepo.findById(new ObjectId(id))
}
