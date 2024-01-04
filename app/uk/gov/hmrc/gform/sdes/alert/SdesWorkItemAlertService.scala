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

package uk.gov.hmrc.gform.sdes.alert

import org.slf4j.{ Logger, LoggerFactory }
import uk.gov.hmrc.gform.email.EmailService
import uk.gov.hmrc.gform.scheduler.datastore.DataStoreWorkItemRepo
import uk.gov.hmrc.gform.scheduler.dms.DmsWorkItemRepo
import uk.gov.hmrc.gform.scheduler.quartz.QScheduledService
import uk.gov.hmrc.gform.sharedmodel.email.EmailTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailParameterValue, EmailParametersRecalculated, EmailTemplateVariable }
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierEmailAddress
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.lock.{ LockService, MongoLockRepository }
import uk.gov.hmrc.mongo.workitem.ProcessingStatus

import scala.concurrent.duration.Duration
import scala.concurrent.{ ExecutionContext, Future }

class SdesWorkItemAlertService(
  notifierEmailAddress: NotifierEmailAddress,
  emailTemplateId: EmailTemplateId,
  emailService: EmailService,
  dmsWorkItemRepo: DmsWorkItemRepo,
  dataStoreWorkItemRepo: DataStoreWorkItemRepo,
  lockRepositoryProvider: MongoLockRepository,
  mongodbLockTimeoutDuration: Duration
) extends QScheduledService[Unit] {
  private val logger: Logger = LoggerFactory.getLogger(getClass)
  private val jobName = "SdesWorkItemAlertJob"

  override def invoke(implicit ec: ExecutionContext): Future[Unit] = {

    val lockKeeper = LockService(
      lockRepository = lockRepositoryProvider,
      lockId = jobName,
      ttl = mongodbLockTimeoutDuration
    )

    lockKeeper
      .withLock(processUnAlertedItems)
      .map {
        case Some(result) => result
        case None =>
          logger.info(s"$jobName locked because it might be running on another instance")
      }
      .recover { case e: Exception =>
        logger.error(s"$jobName failed with exception", e)
      }
  }

  private def processUnAlertedItems(implicit ec: ExecutionContext): Future[Unit] =
    for {
      dmsFailedCount                  <- dmsWorkItemRepo.count(ProcessingStatus.Failed)
      dmsPermanentlyFailedCount       <- dmsWorkItemRepo.count(ProcessingStatus.PermanentlyFailed)
      dataStoreFailedCount            <- dataStoreWorkItemRepo.count(ProcessingStatus.Failed)
      dataStorePermanentlyFailedCount <- dataStoreWorkItemRepo.count(ProcessingStatus.PermanentlyFailed)
      total = dmsFailedCount + dmsPermanentlyFailedCount + dataStoreFailedCount + dataStorePermanentlyFailedCount
      _ <- if (total > 0) sendEmail(total)
           else {
             logger.info("No failed SDES work item was found")
             Future.unit
           }
    } yield ()

  private def sendEmail(total: Long)(implicit ec: ExecutionContext): Future[Unit] = {
    val emailParametersRecalculated = EmailParametersRecalculated(
      Map(
        EmailTemplateVariable("total") -> EmailParameterValue(total.toString)
      )
    )
    logger.info(s"Alert is sending for SDES work-item. Total item : $total")
    implicit val hc = new HeaderCarrier()
    emailService.sendEmail(Some(notifierEmailAddress.value), emailTemplateId, emailParametersRecalculated)
  }
}
