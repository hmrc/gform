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

import org.slf4j.{ Logger, LoggerFactory }
import uk.gov.hmrc.gform.sdes.SdesAlgebra
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination
import uk.gov.hmrc.gform.sharedmodel.sdes.NotificationStatus.FileReady
import uk.gov.hmrc.gform.scheduler.quartz.QScheduledService
import uk.gov.hmrc.mongo.lock.{ LockService, MongoLockRepository }
import scala.concurrent.duration.Duration
import scala.concurrent.{ ExecutionContext, Future }
import play.api.libs.ws.WSClient
import cats.implicits._

class SdesReNotifyService(
  renotifyDestination: Seq[SdesDestination],
  wsClient: WSClient,
  sdesAlgebra: SdesAlgebra[Future],
  lockRepositoryProvider: MongoLockRepository,
  mongodbLockTimeoutDuration: Duration,
  showBeforeLastUpdatedAt: Option[Int],
  gformBaseUrl: String
) extends QScheduledService[Unit] {
  private val logger: Logger = LoggerFactory.getLogger(getClass)
  private val jobName = "ReNotifyJob"

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
        logger.info(s"$jobName failed with exception", e)
      }
  }

  private def renotifyItems(implicit ec: ExecutionContext): Future[Unit] =
    Future.sequence(renotifyDestination.map(renotifyItemsFor)).map(_ => ())

  private def renotifyItemsFor(destination: SdesDestination)(implicit ec: ExecutionContext): Future[Unit] =
    for {
      sdesSubmissionsData <-
        sdesAlgebra.searchAll(None, None, Some(FileReady), None, Some(destination), showBeforeLastUpdatedAt)
      _ <- sdesSubmissionsData.sdesSubmissions.toList.traverse { submission =>
             val url = gformBaseUrl + uk.gov.hmrc.gform.sdes.routes.SdesController
               .renotifySDES(submission.correlationId)
               .url
             val request = wsClient.url(url)
             request.post("").map { response =>
               if (response.status == play.api.http.Status.OK) {
                 logger.info(s"Notification successfully updated for ID: ${submission.correlationId.value}.")
               } else {
                 logger.error(s"Unable to renotify ID: ${submission.correlationId.value}.")
               }
             }
           }
    } yield ()

}
