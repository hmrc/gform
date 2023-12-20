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

import cats.syntax.eq._
import cats.syntax.traverse._
import cats.syntax.functor._
import cats.syntax.show._
import com.mongodb.client.result.UpdateResult
import org.mongodb.scala.bson.{ BsonArray, BsonDocument }
import org.mongodb.scala.model.{ Accumulators, Aggregates, Field, Filters, Sorts }
import org.mongodb.scala.model.Filters.{ equal, exists, lt }
import org.slf4j.LoggerFactory
import play.api.libs.json.{ Json, OFormat }
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.envelope.EnvelopeAlgebra
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sdes.SdesConfig
import uk.gov.hmrc.gform.sdes.datastore.DataStoreWorkItemAlgebra
import uk.gov.hmrc.gform.sdes.dms.DmsWorkItemAlgebra
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.sdes.NotificationStatus.{ FileReady, fromName }
import uk.gov.hmrc.gform.sharedmodel.sdes._
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.mongo.play.json.Codecs
import uk.gov.hmrc.objectstore.client.ObjectSummaryWithMd5

import java.time.{ Instant, LocalDateTime }
import scala.concurrent.{ ExecutionContext, Future }

trait SdesAlgebra[F[_]] {

  def notifySDES(
    correlationId: CorrelationId,
    envelopeId: EnvelopeId,
    formTemplateId: FormTemplateId,
    submissionRef: SubmissionRef,
    notifyRequest: SdesNotifyRequest,
    destination: SdesDestination
  )(implicit
    hc: HeaderCarrier
  ): F[HttpResponse]

  def renotifySDES(sdesSubmission: SdesSubmission, objWithSummary: ObjectSummaryWithMd5)(implicit
    hc: HeaderCarrier
  ): F[HttpResponse]

  def saveSdesSubmission(sdesSubmission: SdesSubmission): F[Unit]

  def findSdesSubmission(correlationId: CorrelationId): F[Option[SdesSubmission]]

  def findSdesSubmissionByEnvelopeId(envelopeId: EnvelopeId): F[List[SdesSubmission]]

  def search(
    page: Int,
    pageSize: Int,
    processed: Option[Boolean],
    formTemplateId: Option[FormTemplateId],
    status: Option[NotificationStatus],
    showBeforeAt: Option[Boolean],
    destination: Option[SdesDestination]
  ): F[SdesSubmissionPageData]

  def searchAll(
    processed: Option[Boolean],
    formTemplateId: Option[FormTemplateId],
    status: Option[NotificationStatus],
    showBeforeAt: Option[Boolean],
    destination: Option[SdesDestination],
    showBeforeLastUpdatedAt: Option[Int]
  ): F[SdesSubmissionPageData]

  def updateAsManualConfirmed(correlation: CorrelationId): F[Unit]

  def getSdesSubmissionsDestination(): F[Seq[SdesSubmissionsStats]]

  def sdesMigration(from: String, to: String): F[UpdateResult]

}

class SdesService(
  sdesConnector: SdesConnector,
  repoSdesSubmission: Repo[SdesSubmission],
  dmsWorkItemAlgebra: DmsWorkItemAlgebra[Future],
  dataStoreWorkItemAlgebra: DataStoreWorkItemAlgebra[Future],
  envelopeAlgebra: EnvelopeAlgebra[Future],
  sdesConfig: SdesConfig
)(implicit
  ec: ExecutionContext
) extends SdesAlgebra[Future] {
  private val logger = LoggerFactory.getLogger(getClass)

  override def notifySDES(
    correlationId: CorrelationId,
    envelopeId: EnvelopeId,
    formTemplateId: FormTemplateId,
    submissionRef: SubmissionRef,
    notifyRequest: SdesNotifyRequest,
    destination: SdesDestination
  )(implicit
    hc: HeaderCarrier
  ): Future[HttpResponse] = {
    val sdesSubmission =
      SdesSubmission.createSdesSubmission(correlationId, envelopeId, formTemplateId, submissionRef, destination)

    val sdesRouting = sdesSubmission.sdesDestination.sdesRouting(sdesConfig)

    for {
      res <- sdesConnector.notifySDES(notifyRequest, sdesRouting)
      _   <- saveSdesSubmission(sdesSubmission.copy(submittedAt = Some(Instant.now), status = FileReady))
    } yield res
  }

  override def saveSdesSubmission(sdesSubmission: SdesSubmission): Future[Unit] =
    repoSdesSubmission.upsert(sdesSubmission).toFuture

  override def findSdesSubmission(correlationId: CorrelationId): Future[Option[SdesSubmission]] =
    repoSdesSubmission.find(correlationId.value)

  override def findSdesSubmissionByEnvelopeId(envelopeId: EnvelopeId): Future[List[SdesSubmission]] = {
    val query = Filters.equal("envelopeId", envelopeId.value)
    repoSdesSubmission.search(query)
  }

  override def search(
    page: Int,
    pageSize: Int,
    processed: Option[Boolean],
    formTemplateId: Option[FormTemplateId],
    status: Option[NotificationStatus],
    showBeforeAt: Option[Boolean],
    destination: Option[SdesDestination]
  ): Future[SdesSubmissionPageData] = {
    val skip = page * pageSize
    doSearch(Some((skip, pageSize)), processed, formTemplateId, status, showBeforeAt, destination, None)
  }

  override def searchAll(
    processed: Option[Boolean],
    formTemplateId: Option[FormTemplateId],
    status: Option[NotificationStatus],
    showBeforeAt: Option[Boolean],
    destination: Option[SdesDestination],
    showBeforeLastUpdatedAt: Option[Int]
  ): Future[SdesSubmissionPageData] =
    doSearch(None, processed, formTemplateId, status, showBeforeAt, destination, showBeforeLastUpdatedAt)

  private def doSearch(
    maybeSkipAndPageSize: Option[(Int, Int)],
    processed: Option[Boolean],
    formTemplateId: Option[FormTemplateId],
    status: Option[NotificationStatus],
    showBeforeAt: Option[Boolean],
    destination: Option[SdesDestination],
    showBeforeLastUpdatedAt: Option[Int]
  ): Future[SdesSubmissionPageData] = {

    val queryByTemplateId =
      formTemplateId.fold(exists("_id"))(t => equal("formTemplateId", t.value))
    val queryByProcessed =
      processed.fold(queryByTemplateId)(p => Filters.and(equal("isProcessed", p), queryByTemplateId))
    val queryByStatus =
      status.fold(queryByProcessed)(s => Filters.and(equal("status", fromName(s)), queryByProcessed))

    val queryByLastUpdated =
      showBeforeLastUpdatedAt.fold(queryByStatus)(d =>
        Filters.and(lt("lastUpdated", LocalDateTime.now().minusHours(d.toLong)), queryByStatus)
      )

    val queryByDestination =
      destination.fold(queryByLastUpdated)(d =>
        Filters.and(equal("destination", SdesDestination.fromName(d)), queryByStatus)
      )
    val query = if (showBeforeAt.getOrElse(false)) {
      Filters.and(
        queryByDestination,
        Filters.and(lt("createdAt", LocalDateTime.now().minusHours(10)), equal("isProcessed", false))
      )
    } else {
      queryByDestination
    }

    val sort = equal("createdAt", -1)

    for {
      sdesSubmissions <- maybeSkipAndPageSize match {
                           case Some((skip, pageSize)) =>
                             repoSdesSubmission.page(query, sort, skip, pageSize)
                           case None => repoSdesSubmission.search(query, sort)
                         }
      sdesSubmissionData <-
        sdesSubmissions.traverse(sdesSubmission =>
          for {
            (numberOfFiles, uploadCount, size) <-
              sdesSubmission.sdesDestination match {
                case SdesDestination.Dms =>
                  val envelope = envelopeAlgebra.get(sdesSubmission.envelopeId)
                  envelope.map(e =>
                    (
                      e.files.count(f => f.fileId =!= FileUploadService.FileIds.dataStore.value),
                      e.files.count(f => !FileUploadService.FileIds.generatedFileIds.map(_.value).contains(f.fileId)),
                      e.files.map(_.length).sum
                    )
                  )
                case _ => Future.successful((1, 0, 0L))
              }
          } yield SdesSubmissionData.fromSdesSubmission(sdesSubmission, numberOfFiles, uploadCount, size)
        )
      count <- repoSdesSubmission.count(query)
    } yield SdesSubmissionPageData(sdesSubmissionData, count)

  }

  override def renotifySDES(sdesSubmission: SdesSubmission, objWithSummary: ObjectSummaryWithMd5)(implicit
    hc: HeaderCarrier
  ): Future[HttpResponse] = {
    val sdesDestination = sdesSubmission.sdesDestination
    val sdesRouting = sdesDestination.sdesRouting(sdesConfig)
    val notifyRequest = sdesDestination match {
      case SdesDestination.DataStore | SdesDestination.DataStoreLegacy | SdesDestination.HmrcIlluminate =>
        dataStoreWorkItemAlgebra.createNotifyRequest(objWithSummary, sdesSubmission._id.value, sdesRouting)
      case SdesDestination.Dms =>
        dmsWorkItemAlgebra.createNotifyRequest(objWithSummary, sdesSubmission._id.value)
    }
    for {
      res <- sdesConnector.notifySDES(notifyRequest, sdesRouting)
      _ <-
        saveSdesSubmission(
          sdesSubmission.copy(
            submittedAt = Some(sdesSubmission.submittedAt.getOrElse(Instant.now)),
            lastUpdated = Some(Instant.now),
            isProcessed = false,
            status = FileReady,
            failureReason = None,
            confirmedAt = None
          )
        )
    } yield res
  }

  override def updateAsManualConfirmed(
    correlationId: CorrelationId
  ): Future[Unit] =
    for {
      sdesSubmission <- repoSdesSubmission.get(correlationId.value)
      _ <- saveSdesSubmission(
             sdesSubmission.copy(
               status = NotificationStatus.FileProcessedManualConfirmed,
               lastUpdated = Some(Instant.now),
               isProcessed = true,
               confirmedAt = Some(Instant.now)
             )
           ).as(logger.info(show"SdesService.updateAsManualConfirmed(${correlationId.value}) - updated manually)"))
    } yield ()

  override def getSdesSubmissionsDestination(): Future[Seq[SdesSubmissionsStats]] = {
    val group = Aggregates.group(
      "$destination",
      Accumulators.sum("count", 1)
    )
    val set = Aggregates.set(
      Field("destination", "$_id")
    )
    val unset = BsonDocument("$unset" -> BsonArray("_id"))
    val sort = Aggregates.sort(Sorts.descending("count"))
    val pipeline = List(group, set, unset, sort)
    repoSdesSubmission
      .aggregate(pipeline)
      .map(_.map(Codecs.fromBson[SdesSubmissionsStats]))
  }

  override def sdesMigration(from: String, to: String): Future[UpdateResult] =
    repoSdesSubmission.sdesMigration(from, to)
}

final case class SdesSubmissionsStats(destination: String, count: Long)

object SdesSubmissionsStats {
  implicit val format: OFormat[SdesSubmissionsStats] = Json.format
}
