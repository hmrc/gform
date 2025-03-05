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
import org.apache.pekko.stream.Materializer
import org.apache.pekko.util.ByteString
import org.mongodb.scala.bson.{ BsonArray, BsonDocument }
import org.mongodb.scala.model.{ Accumulators, Aggregates, Field, Filters, Sorts }
import org.mongodb.scala.model.Filters.{ equal, lt }
import org.slf4j.LoggerFactory
import play.api.libs.json.{ Json, OFormat }
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.envelope.EnvelopeAlgebra
import uk.gov.hmrc.gform.history.DateFilter
import uk.gov.hmrc.gform.objectstore.{ ObjectStoreAlgebra, ObjectStoreService }
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sdes.workitem.DestinationWorkItemAlgebra
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.sdes.NotificationStatus.{ FileProcessed, FileReady, Replaced, fromName }
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

  def search(sdesFilter: SdesFilter): F[SdesSubmissionPageData]

  def searchAll(
    processed: Option[Boolean],
    searchKey: Option[String],
    status: Option[NotificationStatus],
    destination: Option[SdesDestination],
    beforeSubmittedAt: Option[Int]
  ): F[SdesSubmissionPageData]

  def updateAsManualConfirmed(correlation: CorrelationId): F[Unit]

  def getSdesSubmissionsDestination(): F[Seq[SdesSubmissionsStats]]

  def sdesMigration(from: String, to: String): F[UpdateResult]

  def update(notification: CallBackNotification)(implicit hc: HeaderCarrier): F[Unit]

  def resend(correlationId: CorrelationId)(implicit hc: HeaderCarrier): F[Unit]
}

class SdesService(
  sdesConnector: SdesConnector,
  repoSdesSubmission: Repo[SdesSubmission],
  destinationWorkItemAlgebra: DestinationWorkItemAlgebra[Future],
  envelopeAlgebra: EnvelopeAlgebra[Future],
  sdesConfig: SdesConfig,
  sdesHistoryAlgebra: SdesHistoryAlgebra[Future],
  objectStoreAlgebra: ObjectStoreAlgebra[Future]
)(implicit
  ec: ExecutionContext,
  m: Materializer
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
    val sdesHistory =
      SdesHistory.create(
        envelopeId,
        correlationId,
        NotificationStatus.FileReady,
        notifyRequest.file.name,
        None,
        Some(notifyRequest)
      )

    for {
      res <- sdesConnector.notifySDES(notifyRequest, sdesRouting)
      _   <- saveSdesSubmission(sdesSubmission.copy(submittedAt = Some(Instant.now), status = FileReady))
      _   <- sdesHistoryAlgebra.save(sdesHistory)
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

  override def search(filter: SdesFilter): Future[SdesSubmissionPageData] = {
    val pageSize = filter.pageSize
    val skip = filter.page * pageSize
    doSearch(
      Some((skip, pageSize)),
      filter.isProcessed,
      filter.searchKey,
      filter.status,
      filter.destination,
      None,
      filter.from,
      filter.to
    )
  }

  override def searchAll(
    processed: Option[Boolean],
    searchKey: Option[String],
    status: Option[NotificationStatus],
    destination: Option[SdesDestination],
    beforeSubmittedAt: Option[Int]
  ): Future[SdesSubmissionPageData] =
    doSearch(None, processed, searchKey, status, destination, beforeSubmittedAt, None, None)

  private def doSearch(
    maybeSkipAndPageSize: Option[(Int, Int)],
    isProcessed: Option[Boolean],
    searchKey: Option[String],
    status: Option[NotificationStatus],
    destination: Option[SdesDestination],
    beforeSubmittedAt: Option[Int],
    from: Option[DateFilter],
    to: Option[DateFilter]
  ): Future[SdesSubmissionPageData] = {

    val UUID_REGEX = "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$".r

    val queryBySearchKey = searchKey.map(key =>
      if (UUID_REGEX.findFirstIn(key).isDefined) {
        equal("envelopeId", key)
      } else if (key.length === 14) {
        equal("submissionRef", key)
      } else {
        equal("formTemplateId", key)
      }
    )

    val queryByProcessed = isProcessed.map(p => equal("isProcessed", p))
    val queryByStatus = status.map(s => equal("status", fromName(s)))
    val queryByDestination = destination.map(d => equal("destination", SdesDestination.fromName(d)))
    val queryByBeforeAt = beforeSubmittedAt.map(b =>
      Filters.and(lt("createdAt", LocalDateTime.now().minusHours(b.toLong)), equal("isProcessed", false))
    )

    val queryByDateFilter = (from, to) match {
      case (Some(from), Some(to)) => Filters.and(from.gte, to.lte)
      case (Some(from), None)     => from.gte
      case (None, Some(to))       => to.lte
      case (None, None)           => Filters.empty()
    }

    val conditions = List(
      queryBySearchKey,
      queryByProcessed,
      queryByStatus,
      queryByDestination,
      queryByBeforeAt,
      Some(queryByDateFilter)
    ).flatten

    val query = Filters.and(conditions: _*)

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
                      e.files.count(f => f.fileId =!= ObjectStoreService.FileIds.dataStore.value),
                      e.files.count(f => !ObjectStoreService.FileIds.generatedFileIds.map(_.value).contains(f.fileId)),
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
    val notifyRequest =
      destinationWorkItemAlgebra.createNotifyRequest(
        objWithSummary,
        sdesSubmission._id.value,
        sdesRouting
      )
    val sdesHistory = SdesHistory.create(
      sdesSubmission.envelopeId,
      sdesSubmission._id,
      FileReady,
      notifyRequest.file.name,
      None,
      Some(notifyRequest)
    )
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
      _ <- sdesHistoryAlgebra.save(sdesHistory)
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

  override def update(notification: CallBackNotification)(implicit hc: HeaderCarrier): Future[Unit] = {
    val CallBackNotification(responseStatus, fileName, correlationID, responseFailureReason) = notification

    repoSdesSubmission
      .find(correlationID)
      .flatMap(
        _.map(submission =>
          if (isLocked(submission)) {
            Future.failed(new RuntimeException(s"Correlation Id: $correlationID is locked"))
          } else {
            val envelopeId = submission.envelopeId
            logger.info(
              s"Received callback for envelopeId: ${envelopeId.value}, destination: ${submission.destination.getOrElse("dms")}"
            )
            if (submission.status === Replaced) {
              logger.error(
                s"Received callback for a replaced submission: correlation id: $correlationID, envelope id ${envelopeId.value}, destination: ${submission.destination
                  .getOrElse("dms")}"
              )
              Future.failed(new IllegalStateException(s"Correlation ID [$correlationID] has already been replaced"))
            } else {
              for {
                _ <- saveSdesSubmission(submission.copy(lockedAt = Some(Instant.now()))) // lock the record
                updatedSdesSubmission = submission.copy(
                                          isProcessed = responseStatus === FileProcessed,
                                          status = responseStatus,
                                          confirmedAt = Some(Instant.now),
                                          failureReason = responseFailureReason,
                                          lockedAt = None
                                        )
                _ <- if (!submission.isProcessed) {
                       for {
                         _ <- saveSdesSubmission(updatedSdesSubmission)
                         _ <- if (responseStatus === FileProcessed) {
                                deleteFiles(submission, fileName)
                              } else Future.unit
                       } yield ()
                     } else saveSdesSubmission(submission.copy(lockedAt = None))
                sdesHistory = SdesHistory.create(
                                envelopeId,
                                CorrelationId(correlationID),
                                responseStatus,
                                fileName,
                                responseFailureReason,
                                None
                              )
                _ <- sdesHistoryAlgebra.save(sdesHistory)
              } yield ()
            }
          }
        ).getOrElse(
          Future.failed(new RuntimeException(s"Correlation id [$correlationID] not found in mongo collection"))
        )
      )
  }
  private def deleteFiles(submission: SdesSubmission, fileName: String)(implicit hc: HeaderCarrier): Future[Unit] = {
    val sdesDestination = submission.sdesDestination
    val envelopeId = submission.envelopeId
    val paths = sdesDestination.objectStorePaths(envelopeId)
    sdesDestination match {
      case SdesDestination.DataStore | SdesDestination.DataStoreLegacy | SdesDestination.HmrcIlluminate =>
        objectStoreAlgebra.deleteFile(paths.ephemeral, fileName)
      case SdesDestination.Dms | SdesDestination.InfoArchive => objectStoreAlgebra.deleteZipFile(envelopeId, paths)
    }
  }

  private def isLocked(submission: SdesSubmission): Boolean =
    submission.lockedAt.exists(_.isAfter(Instant.now().minusMillis(sdesConfig.lockTTL)))

  override def resend(correlationId: CorrelationId)(implicit hc: HeaderCarrier): Future[Unit] =
    for {
      sdesSubmission <- findSdesSubmission(correlationId)
      result <- sdesSubmission match {
                  case Some(submission) =>
                    val sdesDestination = submission.sdesDestination
                    val paths = sdesDestination.objectStorePaths(submission.envelopeId)
                    for {
                      objSummary <- sdesDestination match {
                                      case SdesDestination.DataStore | SdesDestination.DataStoreLegacy |
                                          SdesDestination.HmrcIlluminate =>
                                        val fileName = s"${submission.envelopeId.value}.json"
                                        for {
                                          maybeObject <- objectStoreAlgebra.getFile(paths.permanent, fileName)
                                          objSummary <- maybeObject match {
                                                          case Some(obj) =>
                                                            val byteStringFuture: Future[ByteString] =
                                                              obj.content.runFold(ByteString.empty)(_ ++ _)

                                                            byteStringFuture.flatMap { concatenatedByteString =>
                                                              objectStoreAlgebra.uploadFileWithDir(
                                                                paths.ephemeral,
                                                                fileName,
                                                                concatenatedByteString,
                                                                ContentType.`application/json`
                                                              )
                                                            }
                                                          case None =>
                                                            Future.failed(
                                                              new Exception(
                                                                s"File ${submission.envelopeId.value}.json not found in the object store."
                                                              )
                                                            )
                                                        }
                                        } yield objSummary
                                      case SdesDestination.Dms =>
                                        objectStoreAlgebra.zipFiles(submission.envelopeId, paths)
                                      case SdesDestination.InfoArchive =>
                                        objectStoreAlgebra.zipAndEncrypt(submission.envelopeId, paths)
                                    }
                      res <- createWorkItem(submission, objSummary)
                    } yield res
                  case None =>
                    Future.failed(
                      new RuntimeException(s"Correlation id [$correlationId] not found in mongo collection")
                    )
                }
    } yield result

  private def createWorkItem(sdesSubmission: SdesSubmission, objWithSummary: ObjectSummaryWithMd5): Future[Unit] = {
    val sdesDestination = sdesSubmission.sdesDestination
    for {
      _ <- destinationWorkItemAlgebra.pushWorkItem(
             sdesSubmission.envelopeId,
             sdesSubmission.formTemplateId,
             sdesSubmission.submissionRef,
             objWithSummary,
             sdesDestination
           )
      _ <-
        saveSdesSubmission(
          sdesSubmission.copy(
            lastUpdated = Some(Instant.now),
            isProcessed = true,
            status = Replaced
          )
        )
    } yield ()
  }
}

final case class SdesSubmissionsStats(destination: String, count: Long)

object SdesSubmissionsStats {
  implicit val format: OFormat[SdesSubmissionsStats] = Json.format
}
