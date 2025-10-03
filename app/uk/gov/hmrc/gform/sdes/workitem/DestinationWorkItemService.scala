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

package uk.gov.hmrc.gform.sdes.workitem

import cats.syntax.functor._
import org.bson.types.ObjectId
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Filters.equal
import uk.gov.hmrc.gform.scheduler.datastore.DataStoreWorkItemRepo
import uk.gov.hmrc.gform.scheduler.dms.DmsWorkItemRepo
import uk.gov.hmrc.gform.scheduler.infoarchive.InfoArchiveWorkItemRepo
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.sdes._
import uk.gov.hmrc.mongo.workitem.{ ProcessingStatus, WorkItem }

import java.util.UUID
import scala.concurrent.{ ExecutionContext, Future }

trait DestinationWorkItemAlgebra[F[_]] {
  def pushWorkItem(
    envelopeId: EnvelopeId,
    formTemplateId: FormTemplateId,
    submissionRef: SubmissionRef,
    destination: SdesDestination,
    filePrefix: Option[String]
  ): F[Unit]

  def search(
    page: Int,
    pageSize: Int,
    sdesDestination: SdesDestination,
    formTemplateId: Option[FormTemplateId],
    status: Option[ProcessingStatus]
  ): F[SdesWorkItemPageData]

  def enqueue(id: String, sdesDestination: SdesDestination): F[Unit]

  def find(id: String, sdesDestination: SdesDestination): F[Option[WorkItem[SdesWorkItem]]]

  def findByEnvelopeId(envelopeId: EnvelopeId, sdesDestination: SdesDestination): F[List[WorkItem[SdesWorkItem]]]

  def delete(id: String, sdesDestination: SdesDestination): F[Unit]
}

class DestinationWorkItemService(
  dmsWorkItemRepo: DmsWorkItemRepo,
  dataStoreWorkItemRepo: DataStoreWorkItemRepo,
  infoArchiveWorkItemRepo: InfoArchiveWorkItemRepo
)(implicit ec: ExecutionContext)
    extends DestinationWorkItemAlgebra[Future] {
  override def pushWorkItem(
    envelopeId: EnvelopeId,
    formTemplateId: FormTemplateId,
    submissionRef: SubmissionRef,
    sdesDestination: SdesDestination,
    filePrefix: Option[String]
  ): Future[Unit] = {
    val correlationId = UUID.randomUUID().toString
    val sdesWorkItem =
      SdesWorkItem(
        CorrelationId(correlationId),
        envelopeId,
        formTemplateId,
        submissionRef,
        sdesDestination,
        filePrefix
      )
    sdesDestination match {
      case SdesDestination.Dms => dmsWorkItemRepo.pushNew(sdesWorkItem).void
      case SdesDestination.HmrcIlluminate | SdesDestination.DataStore | SdesDestination.DataStoreLegacy =>
        dataStoreWorkItemRepo.pushNew(sdesWorkItem).void
      case SdesDestination.InfoArchive => infoArchiveWorkItemRepo.pushNew(sdesWorkItem).void
    }
  }

  override def search(
    page: Int,
    pageSize: Int,
    sdesDestination: SdesDestination,
    formTemplateId: Option[FormTemplateId],
    status: Option[ProcessingStatus]
  ): Future[SdesWorkItemPageData] = {
    val queryByFormTemplateId = formTemplateId.fold(Filters.exists("_id"))(t => equal("item.formTemplateId", t.value))
    val query = status.fold(queryByFormTemplateId)(s => Filters.and(equal("status", s.name), queryByFormTemplateId))
    val sort = equal("receivedAt", -1)

    val skip = page * pageSize
    val collection = findCollection(sdesDestination)
    for {
      dmsWorkItem <-
        collection.find(query).sort(sort).skip(skip).limit(pageSize).toFuture().map(_.toList)
      dmsWorkItemData = dmsWorkItem.map(workItem => SdesWorkItemData.fromWorkItem(workItem, 1))
      count <- collection.countDocuments(query).toFuture()
    } yield SdesWorkItemPageData(dmsWorkItemData, count)
  }

  private def findCollection(sdesDestination: SdesDestination) = sdesDestination match {
    case SdesDestination.Dms => dmsWorkItemRepo.collection
    case SdesDestination.HmrcIlluminate | SdesDestination.DataStore | SdesDestination.DataStoreLegacy =>
      dataStoreWorkItemRepo.collection
    case SdesDestination.InfoArchive => infoArchiveWorkItemRepo.collection
  }

  override def delete(id: String, sdesDestination: SdesDestination): Future[Unit] =
    findCollection(sdesDestination)
      .deleteOne(equal("_id", new ObjectId(id)))
      .toFuture()
      .map(_.getDeletedCount > 0)
      .void

  override def enqueue(id: String, sdesDestination: SdesDestination): Future[Unit] =
    sdesDestination match {
      case SdesDestination.Dms => dmsWorkItemRepo.markAs(new ObjectId(id), ProcessingStatus.ToDo).void
      case SdesDestination.HmrcIlluminate | SdesDestination.DataStore | SdesDestination.DataStoreLegacy =>
        dataStoreWorkItemRepo.markAs(new ObjectId(id), ProcessingStatus.ToDo).void
      case SdesDestination.InfoArchive => infoArchiveWorkItemRepo.markAs(new ObjectId(id), ProcessingStatus.ToDo).void
    }

  override def find(id: String, sdesDestination: SdesDestination): Future[Option[WorkItem[SdesWorkItem]]] =
    sdesDestination match {
      case SdesDestination.Dms => dmsWorkItemRepo.findById(new ObjectId(id))
      case SdesDestination.HmrcIlluminate | SdesDestination.DataStore | SdesDestination.DataStoreLegacy =>
        dataStoreWorkItemRepo.findById(new ObjectId(id))
      case SdesDestination.InfoArchive => infoArchiveWorkItemRepo.findById(new ObjectId(id))
    }

  override def findByEnvelopeId(
    envelopeId: EnvelopeId,
    sdesDestination: SdesDestination
  ): Future[List[WorkItem[SdesWorkItem]]] = {
    val query = Filters.equal("item.envelopeId", envelopeId.value)
    findCollection(sdesDestination).find(query).toFuture().map(_.toList)
  }
}
