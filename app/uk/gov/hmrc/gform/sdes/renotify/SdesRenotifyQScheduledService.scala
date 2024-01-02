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

package uk.gov.hmrc.gform.sdes.renotify

import cats.implicits._
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.scheduler.quartz.QScheduledService
import uk.gov.hmrc.gform.sdes.SdesAlgebra
import uk.gov.hmrc.gform.sdes.SdesRenotifyService
import uk.gov.hmrc.gform.sharedmodel.sdes.NotificationStatus.FileReady
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.lock.LockService
import uk.gov.hmrc.mongo.lock.MongoLockRepository

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration

class SdesRenotifyQScheduledService(
  renotifyDestination: Seq[SdesDestination],
  sdesRenotifyService: SdesRenotifyService,
  sdesAlgebra: SdesAlgebra[Future],
  lockRepositoryProvider: MongoLockRepository,
  mongodbLockTimeoutDuration: Duration,
  showBeforeSubmittedAt: Option[Int]
) extends QScheduledService[Unit] {
  private val logger: Logger = LoggerFactory.getLogger(getClass)
  private val jobName = "SdesRenotifyJob"

  override def invoke(implicit ec: ExecutionContext): Future[Unit] = {

    val lockKeeper = LockService(
      lockRepository = lockRepositoryProvider,
      lockId = jobName,
      ttl = mongodbLockTimeoutDuration
    )

    lockKeeper
      .withLock(renotifyItems)
      .map {
        case Some(result) => result
        case None =>
          logger.info(s"$jobName locked because it might be running on another instance")
      }
      .recover { case e: Exception =>
        logger.error(s"$jobName failed with exception", e)
      }
  }

  private def renotifyItems(implicit ec: ExecutionContext): Future[Unit] =
    Future.sequence(renotifyDestination.map(renotifyItemsFor)).map(_ => ())

  private def renotifyItemsFor(destination: SdesDestination)(implicit ec: ExecutionContext): Future[Unit] =
    for {
      sdesSubmissionsData <-
        sdesAlgebra.searchAll(None, None, Some(FileReady), Some(destination), None, showBeforeSubmittedAt)
      _ <- sdesSubmissionsData.sdesSubmissions.toList.traverse { submission =>
             implicit val hc = HeaderCarrier()
             sdesRenotifyService
               .renotifySDES(submission.correlationId)
               .map { result =>
                 val status = result.status
                 if (status >= 200 && status < 300) {
                   logger.info(s"Notification successfully updated for ID: ${submission.correlationId.value}.")
                 } else {

                   logger.error(s"Unable to renotify ID: ${submission.correlationId.value}.")
                 }
               }
           }
    } yield ()

}
