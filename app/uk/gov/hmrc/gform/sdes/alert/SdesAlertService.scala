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

import cats.syntax.traverse._
import cats.syntax.functor._
import cats.instances.future._
import uk.gov.hmrc.gform.core._
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Filters.{ equal, lte, notEqual }
import org.slf4j.{ Logger, LoggerFactory }
import uk.gov.hmrc.gform.email.EmailService
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.scheduler.quartz.QScheduledService
import uk.gov.hmrc.gform.sharedmodel.email.EmailTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailParameterValue, EmailParametersRecalculated, EmailTemplateVariable }
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierEmailAddress
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesSubmission
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDateTime
import scala.concurrent.{ ExecutionContext, Future }

class SdesAlertService(
  alertSdesDestination: Option[String],
  notifierEmailAddress: NotifierEmailAddress,
  emailTemplateId: EmailTemplateId,
  emailService: EmailService,
  repoSdesSubmission: Repo[SdesSubmission]
) extends QScheduledService[Unit] {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  override def invoke(implicit ec: ExecutionContext): Future[Unit] = {

    val query =
      Filters.and(equal("isProcessed", false), lte("submittedAt", LocalDateTime.now().minusMinutes(30)))

    val totalItemQuery =
      alertSdesDestination.fold(query)(destination => Filters.and(query, equal("destination", destination)))
    val newItemQuery = Filters.and(totalItemQuery, notEqual("isAlerted", true))

    for {
      sdesSubmissions <- repoSdesSubmission.search(newItemQuery)
      _ <- if (sdesSubmissions.nonEmpty) {
             for {
               _ <- sdesSubmissions.traverse(s =>
                      repoSdesSubmission
                        .upsert(s.copy(isAlerted = Some(true)))
                        .toFuture
                        .as(logger.info(s"Alerted SDES submission. Envelope id : ${s.envelopeId.value}"))
                    )
               total <- repoSdesSubmission.search(totalItemQuery)
               _     <- sendEmail(sdesSubmissions.size, total.size)
             } yield ()
           } else {
             logger.info("No new SDES submission found.")
             Future.successful(())
           }
    } yield ()
  }

  private def sendEmail(newItemCount: Int, total: Int)(implicit ec: ExecutionContext): Future[Unit] = {
    val emailParametersRecalculated = EmailParametersRecalculated(
      Map(
        EmailTemplateVariable("newItemCount") -> EmailParameterValue(newItemCount.toString),
        EmailTemplateVariable("total")        -> EmailParameterValue(total.toString)
      )
    )
    logger.info(s"Alert is sending for SDES submission. New item count : $newItemCount, total : $total")
    implicit val hc = new HeaderCarrier()
    emailService.sendEmail(Some(notifierEmailAddress.value), emailTemplateId, emailParametersRecalculated)
  }
}