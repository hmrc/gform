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
import cats.syntax.traverse._
import org.bson.types.ObjectId
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Filters.equal
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.scheduler.asynchandlebars.AsyncHandlebarsWorkItemRepo
import uk.gov.hmrc.gform.scheduler.datalakehouse.DataLakehouseWorkItemRepo
import uk.gov.hmrc.gform.scheduler.datastore.DataStoreWorkItemRepo
import uk.gov.hmrc.gform.scheduler.dms.DmsWorkItemRepo
import uk.gov.hmrc.gform.scheduler.infoarchive.InfoArchiveWorkItemRepo
import uk.gov.hmrc.gform.scheduler.nrsOrchestrator.NrsOrchestratorWorkItemRepo
import uk.gov.hmrc.gform.scheduler.{ TraceableWorkItem, WorkItemRepo }
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ AsyncHandlebarsDestinationResponse, DestinationResponse, NrsOrchestratorDestinationResponse }
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination.AsyncHandlebars
import uk.gov.hmrc.gform.sharedmodel.sdes._
import uk.gov.hmrc.mongo.workitem.ProcessingStatus.ToDo
import uk.gov.hmrc.mongo.workitem.{ ProcessingStatus, WorkItem }

import java.util.UUID
import scala.concurrent.{ ExecutionContext, Future }

trait DestinationWorkItemAlgebra[F[_]] {
  def pushWorkItem(
    envelopeId: EnvelopeId,
    formTemplateId: FormTemplateId,
    submissionRef: SubmissionRef,
    destination: SdesDestination,
    filePrefix: Option[String],
    submissionPrefix: Option[String],
    readyImmediately: Boolean
  ): F[ObjectId]

  def search(
    page: Int,
    pageSize: Int,
    sdesDestination: SdesDestination,
    formTemplateId: Option[FormTemplateId],
    status: Option[ProcessingStatus]
  ): F[SdesWorkItemPageData]

  def readySdes(workItems: List[(SdesDestination, ObjectId)]): F[Unit]

  def enqueue(id: String, sdesDestination: SdesDestination): F[Unit]

  def find(id: String, sdesDestination: SdesDestination): F[Option[WorkItem[SdesWorkItem]]]

  def findTraceableWorkItem(id: String, sdesDestination: SdesDestination): F[Option[WorkItem[TraceableWorkItem[_]]]]

  def findByEnvelopeId(envelopeId: EnvelopeId, sdesDestination: SdesDestination): F[List[WorkItem[SdesWorkItem]]]

  def deleteSdes(id: String, sdesDestination: SdesDestination): F[Unit]

  def readyGeneric(responses: List[DestinationResponse]): F[Unit]

  def deleteGeneric(response: DestinationResponse): F[Unit]
}

class DestinationWorkItemService(
  dmsWorkItemRepo: DmsWorkItemRepo,
  dataStoreWorkItemRepo: DataStoreWorkItemRepo,
  infoArchiveWorkItemRepo: InfoArchiveWorkItemRepo,
  dataLakehouseWorkItemRepo: DataLakehouseWorkItemRepo,
  nrsOrchestratorWorkItemRepo: NrsOrchestratorWorkItemRepo,
  asyncHandlebarsWorkItemRepo: AsyncHandlebarsWorkItemRepo
)(implicit ec: ExecutionContext)
    extends DestinationWorkItemAlgebra[Future] {

  private val logger = LoggerFactory.getLogger(getClass)

  override def pushWorkItem(
    envelopeId: EnvelopeId,
    formTemplateId: FormTemplateId,
    submissionRef: SubmissionRef,
    sdesDestination: SdesDestination,
    filePrefix: Option[String],
    submissionPrefix: Option[String],
    readyImmediately: Boolean
  ): Future[ObjectId] = {
    val correlationId = UUID.randomUUID().toString
    val sdesWorkItem =
      SdesWorkItem(
        CorrelationId(correlationId),
        envelopeId,
        formTemplateId,
        submissionRef,
        sdesDestination,
        filePrefix,
        submissionPrefix
      )

    val initialState: SdesWorkItem => ProcessingStatus = if (readyImmediately) todo else deferred

    sdesDestination match {
      case SdesDestination.Dms | SdesDestination.PegaCaseflow =>
        dmsWorkItemRepo.pushNew(sdesWorkItem, initialState = initialState).map(_.id)
      case SdesDestination.HmrcIlluminate | SdesDestination.DataStore | SdesDestination.DataStoreLegacy =>
        dataStoreWorkItemRepo.pushNew(sdesWorkItem, initialState = initialState).map(_.id)
      case SdesDestination.InfoArchive =>
        infoArchiveWorkItemRepo.pushNew(sdesWorkItem, initialState = initialState).map(_.id)
      case SdesDestination.DataLakehouse =>
        dataLakehouseWorkItemRepo.pushNew(sdesWorkItem, initialState = initialState).map(_.id)
      case SdesDestination.AsyncHandlebars =>
        logger.error(s"Unsupported SDES destination: $sdesDestination")
        Future.failed(new IllegalArgumentException(s"Unsupported SDES destination: $sdesDestination"))
    }
  }

  private def deferred(item: SdesWorkItem): ProcessingStatus = ProcessingStatus.Deferred
  private def todo(item: SdesWorkItem): ProcessingStatus = ProcessingStatus.ToDo

  override def readySdes(workItems: List[(SdesDestination, ObjectId)]): Future[Unit] =
    workItems
      .traverse {
        case (SdesDestination.Dms | SdesDestination.PegaCaseflow, oid) => dmsWorkItemRepo.markAs(oid, ToDo)
        case (SdesDestination.HmrcIlluminate | SdesDestination.DataStore | SdesDestination.DataStoreLegacy, oid) =>
          dataStoreWorkItemRepo.markAs(oid, ToDo)
        case (SdesDestination.InfoArchive, oid)   => infoArchiveWorkItemRepo.markAs(oid, ToDo)
        case (SdesDestination.DataLakehouse, oid) => dataLakehouseWorkItemRepo.markAs(oid, ToDo)
        case (other, _) =>
          logger.error(s"Unsupported SDES destination in readySdes: ${SdesDestination.fromName(other)})")
          Future.unit
      }
      .map(_ => ())

  override def search(
    page: Int,
    pageSize: Int,
    sdesDestination: SdesDestination,
    formTemplateId: Option[FormTemplateId],
    status: Option[ProcessingStatus]
  ): Future[SdesWorkItemPageData] = {
    val queryByFormTemplateId = formTemplateId.fold(Filters.exists("_id"))(t => equal("item.formTemplateId", t.value))
    val query: Bson =
      status.fold(queryByFormTemplateId)(s => Filters.and(equal("status", s.name), queryByFormTemplateId))
    val sort: Bson = equal("receivedAt", -1)

    val skip: Int = page * pageSize

    sdesDestination match {
      case AsyncHandlebars => searchAsyncHandlebars(query, sort, skip, pageSize)
      case _               => searchSdesDestination(sdesDestination, query, sort, skip, pageSize)
    }
  }

  private def searchSdesDestination(
    sdesDestination: SdesDestination,
    query: Bson,
    sort: Bson,
    skip: Int,
    pageSize: Int
  ): Future[SdesWorkItemPageData] = {
    val collection = findCollection(sdesDestination)
    for {
      dmsWorkItem <-
        collection.find(query).sort(sort).skip(skip).limit(pageSize).toFuture().map(_.toList)
      dmsWorkItemData = dmsWorkItem.map(workItem => SdesWorkItemData.fromWorkItem(workItem, 1))
      count <- collection.countDocuments(query).toFuture()
    } yield SdesWorkItemPageData(dmsWorkItemData, count)
  }

  private def searchAsyncHandlebars(query: Bson, sort: Bson, skip: Int, pageSize: Int): Future[SdesWorkItemPageData] =
    for {
      workItems <-
        asyncHandlebarsWorkItemRepo.collection
          .find(query)
          .sort(sort)
          .skip(skip)
          .limit(pageSize)
          .toFuture()
          .map(_.toList)
          .map(_.map(_.asInstanceOf[WorkItem[TraceableWorkItem[_]]]))
      workItemData =
        workItems.map(workItem => SdesWorkItemData.fromTraceableWorkItem(workItem, AsyncHandlebars, 1))
      count <- asyncHandlebarsWorkItemRepo.collection.countDocuments(query).toFuture()
    } yield SdesWorkItemPageData(workItemData, count)

  private def findCollection(sdesDestination: SdesDestination): MongoCollection[WorkItem[SdesWorkItem]] =
    sdesDestination match {
      case SdesDestination.Dms | SdesDestination.PegaCaseflow => dmsWorkItemRepo.collection
      case SdesDestination.HmrcIlluminate | SdesDestination.DataStore | SdesDestination.DataStoreLegacy =>
        dataStoreWorkItemRepo.collection
      case SdesDestination.InfoArchive   => infoArchiveWorkItemRepo.collection
      case SdesDestination.DataLakehouse => dataLakehouseWorkItemRepo.collection
      case SdesDestination.AsyncHandlebars =>
        logger.error(s"Unsupported SDES destination: $sdesDestination")
        throw new IllegalArgumentException(s"Unsupported SDES destination: $sdesDestination")
    }

  override def deleteSdes(id: String, sdesDestination: SdesDestination): Future[Unit] =
    sdesDestination match {
      case AsyncHandlebars =>
        asyncHandlebarsWorkItemRepo.collection
          .deleteOne(equal("_id", new ObjectId(id)))
          .toFuture()
          .map(_.getDeletedCount > 0)
          .void
      case _ =>
        findCollection(sdesDestination)
          .deleteOne(equal("_id", new ObjectId(id)))
          .toFuture()
          .map(_.getDeletedCount > 0)
          .void
    }

  override def enqueue(id: String, sdesDestination: SdesDestination): Future[Unit] =
    sdesDestination match {
      case SdesDestination.Dms | SdesDestination.PegaCaseflow =>
        dmsWorkItemRepo.markAs(new ObjectId(id), ProcessingStatus.ToDo).void
      case SdesDestination.HmrcIlluminate | SdesDestination.DataStore | SdesDestination.DataStoreLegacy =>
        dataStoreWorkItemRepo.markAs(new ObjectId(id), ProcessingStatus.ToDo).void
      case SdesDestination.InfoArchive => infoArchiveWorkItemRepo.markAs(new ObjectId(id), ProcessingStatus.ToDo).void
      case SdesDestination.DataLakehouse =>
        dataLakehouseWorkItemRepo.markAs(new ObjectId(id), ProcessingStatus.ToDo).void
      case SdesDestination.AsyncHandlebars =>
        asyncHandlebarsWorkItemRepo.markAs(new ObjectId(id), ProcessingStatus.ToDo).void
    }

  override def find(id: String, sdesDestination: SdesDestination): Future[Option[WorkItem[SdesWorkItem]]] =
    sdesDestination match {
      case SdesDestination.Dms | SdesDestination.PegaCaseflow =>
        dmsWorkItemRepo.findById(new ObjectId(id))
      case SdesDestination.HmrcIlluminate | SdesDestination.DataStore | SdesDestination.DataStoreLegacy =>
        dataStoreWorkItemRepo.findById(new ObjectId(id))
      case SdesDestination.InfoArchive   => infoArchiveWorkItemRepo.findById(new ObjectId(id))
      case SdesDestination.DataLakehouse => dataLakehouseWorkItemRepo.findById(new ObjectId(id))
      case SdesDestination.AsyncHandlebars =>
        logger.error(s"Unsupported SDES destination: $sdesDestination")
        Future.failed(new IllegalArgumentException(s"Unsupported SDES destination: $sdesDestination"))
    }

  override def findTraceableWorkItem(
    id: String,
    sdesDestination: SdesDestination
  ): Future[Option[WorkItem[TraceableWorkItem[_]]]] =
    sdesDestination match {
      case AsyncHandlebars =>
        asyncHandlebarsWorkItemRepo
          .findById(new ObjectId(id))
          .map(_.map(_.asInstanceOf[WorkItem[TraceableWorkItem[_]]]))
      case _ =>
        logger.error(s"Unsupported SDES destination for traceable work item: $sdesDestination")
        Future.failed(
          new IllegalArgumentException(s"Unsupported SDES destination for traceable work item: $sdesDestination")
        )
    }

  override def findByEnvelopeId(
    envelopeId: EnvelopeId,
    sdesDestination: SdesDestination
  ): Future[List[WorkItem[SdesWorkItem]]] = {
    val query = Filters.equal("item.envelopeId", envelopeId.value)
    findCollection(sdesDestination).find(query).toFuture().map(_.toList)
  }

  override def readyGeneric(responses: List[DestinationResponse]): Future[Unit] =
    responses
      .traverse {
        case a: AsyncHandlebarsDestinationResponse => asyncHandlebarsWorkItemRepo.markAs(a.workItemId, ToDo)
        case n: NrsOrchestratorDestinationResponse => nrsOrchestratorWorkItemRepo.markAs(n.workItemId, ToDo)
        case _                                     => Future.unit
      }
      .map(_ => ())

  override def deleteGeneric(response: DestinationResponse): Future[Unit] = response match {
    case a: AsyncHandlebarsDestinationResponse =>
      deleteFromRepo(a.workItemId, asyncHandlebarsWorkItemRepo, "AsyncHandlebars")
    case n: NrsOrchestratorDestinationResponse =>
      deleteFromRepo(n.workItemId, nrsOrchestratorWorkItemRepo, "NrsOrchestrator")
    case _ => Future.unit
  }

  private def deleteFromRepo(oid: ObjectId, repo: WorkItemRepo[_], identifier: String): Future[Unit] = {
    logger.info(s"Deleting deferred $identifier work item ${oid.toHexString}")
    repo.collection
      .deleteOne(equal("_id", oid))
      .toFuture()
      .map(_.getDeletedCount > 0)
      .void
  }
}
