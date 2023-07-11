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

package uk.gov.hmrc.gform.sdes.dms

import cats.syntax.eq._
import cats.syntax.traverse._
import cats.syntax.functor._
import org.bson.types.ObjectId
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Filters.equal
import uk.gov.hmrc.gform.envelope.EnvelopeAlgebra
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.scheduler.dms.DmsWorkItemRepo
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.sdes.{ CorrelationId, FileAudit, FileChecksum, FileMetaData, SdesNotifyRequest, SdesWorkItem, SdesWorkItemData, SdesWorkItemPageData }
import uk.gov.hmrc.mongo.workitem.{ ProcessingStatus, WorkItem }
import uk.gov.hmrc.objectstore.client.ObjectSummaryWithMd5

import java.util.{ Base64, UUID }
import scala.concurrent.{ ExecutionContext, Future }

trait DmsWorkItemAlgebra[F[_]] {
  def pushWorkItem(
    envelopeId: EnvelopeId,
    formTemplateId: FormTemplateId,
    submissionRef: SubmissionRef,
    objWithSummary: ObjectSummaryWithMd5
  ): F[Unit]

  def createNotifyRequest(
    objSummary: ObjectSummaryWithMd5,
    correlationId: String
  ): SdesNotifyRequest

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

class DmsWorkItemService(
  dmsWorkItemRepo: DmsWorkItemRepo,
  envelopeAlgebra: EnvelopeAlgebra[Future],
  informationType: String,
  recipientOrSender: String,
  fileLocationUrl: String
)(implicit ec: ExecutionContext)
    extends DmsWorkItemAlgebra[Future] {
  override def pushWorkItem(
    envelopeId: EnvelopeId,
    formTemplateId: FormTemplateId,
    submissionRef: SubmissionRef,
    objWithSummary: ObjectSummaryWithMd5
  ): Future[Unit] = {
    val correlationId = UUID.randomUUID().toString
    val sdesNotifyRequest = createNotifyRequest(objWithSummary, correlationId)
    val sdesWorkItem =
      SdesWorkItem(CorrelationId(correlationId), envelopeId, formTemplateId, submissionRef, sdesNotifyRequest)
    dmsWorkItemRepo.pushNew(sdesWorkItem).void
  }

  def createNotifyRequest(
    objSummary: ObjectSummaryWithMd5,
    correlationId: String
  ): SdesNotifyRequest =
    SdesNotifyRequest(
      informationType,
      FileMetaData(
        recipientOrSender,
        objSummary.location.fileName,
        s"$fileLocationUrl${objSummary.location.asUri}",
        FileChecksum(value = Base64.getDecoder.decode(objSummary.contentMd5.value).map("%02x".format(_)).mkString),
        objSummary.contentLength,
        List()
      ),
      FileAudit(correlationId)
    )

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
      dmsWorkItem <-
        dmsWorkItemRepo.collection.find(query).sort(sort).skip(skip).limit(pageSize).toFuture().map(_.toList)
      dmsWorkItemData <-
        dmsWorkItem.traverse(workItem =>
          for {
            numberOfFiles <-
              envelopeAlgebra
                .get(workItem.item.envelopeId)
                .map(_.files.count(_.fileId =!= FileUploadService.FileIds.dataStore.value))
          } yield SdesWorkItemData.fromWorkItem(workItem, numberOfFiles)
        )
      count <- dmsWorkItemRepo.collection.countDocuments(query).toFuture()
    } yield SdesWorkItemPageData(dmsWorkItemData, count)
  }

  override def delete(id: String): Future[Unit] =
    dmsWorkItemRepo.collection.deleteOne(equal("_id", new ObjectId(id))).toFuture().map(_.getDeletedCount > 0).void

  override def enqueue(id: String): Future[Unit] =
    dmsWorkItemRepo.markAs(new ObjectId(id), ProcessingStatus.ToDo).void

  override def find(id: String): Future[Option[WorkItem[SdesWorkItem]]] =
    dmsWorkItemRepo.findById(new ObjectId(id))
}
