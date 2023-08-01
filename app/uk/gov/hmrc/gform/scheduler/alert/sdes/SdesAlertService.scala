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

package uk.gov.hmrc.gform.scheduler.alert.sdes

import org.slf4j.{ Logger, LoggerFactory }
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.email.EmailService
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.scheduler.alert.AlertName.SdesAlert
import uk.gov.hmrc.gform.scheduler.alert.AlertMethod.Email
import uk.gov.hmrc.gform.scheduler.alert.{ Alert, AlertAlgebra }
import uk.gov.hmrc.gform.sdes.SdesAlgebra
import uk.gov.hmrc.gform.sharedmodel.email.EmailTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailParameterValue, EmailParametersRecalculated, EmailTemplateVariable }
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierEmailAddress
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class SdesAlertService(
  sdesAlgebra: SdesAlgebra[Future],
  thresholdLimit: Int,
  destination: Option[String],
  notifierEmailAddress: NotifierEmailAddress,
  emailTemplateId: EmailTemplateId,
  emailService: EmailService,
  repo: Repo[Alert]
)(implicit
  executionContext: ExecutionContext
) extends AlertAlgebra {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  override def execute: Future[Unit] =
    for {
      submissionCount <-
        sdesAlgebra
          .search(0, thresholdLimit + 1, None, None, None, None, destination.map(SdesDestination.fromString))
          .map(_.count)
      _ <- if (submissionCount >= thresholdLimit) {
             logger.info(s"$submissionCount submissions have not been processed.Threshold limit is $thresholdLimit")
             sendEmail(submissionCount)
           } else {
             logger.info(s"SdesAlert deleting : ${SdesAlert.toString}")
             repo.delete(SdesAlert.toString).toFuture
           }
    } yield ()

  private def sendEmail(submissionCount: Long): Future[Unit] =
    for {
      reported <- repo.find(SdesAlert.toString)
      _ <- if (reported.isEmpty) {
             val emailParametersRecalculated = EmailParametersRecalculated(
               Map(EmailTemplateVariable("submissionCount") -> EmailParameterValue(submissionCount.toString))
             )
             logger.info(s"SDES submission alert is sending")
             implicit val hc = new HeaderCarrier()
             val alert = Alert(SdesAlert, Email, notifierEmailAddress, emailTemplateId, emailParametersRecalculated)
             emailService.sendEmail(Some(notifierEmailAddress.value), emailTemplateId, emailParametersRecalculated)
             repo.upsert(alert).toFuture
           } else Future.unit
    } yield ()
}
