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

import akka.actor.Scheduler
import org.slf4j.LoggerFactory
import play.api.mvc.{ Action, ControllerComponents }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.fileupload.Retrying
import uk.gov.hmrc.gform.sharedmodel.sdes.NotificationStatus.fromName
import uk.gov.hmrc.gform.sharedmodel.sdes.CallBackNotification

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ ExecutionContext, Future }

class SdesCallbackController(
  cc: ControllerComponents,
  sdesAlgebra: SdesAlgebra[Future]
)(implicit ex: ExecutionContext, schduler: Scheduler)
    extends BaseController(cc) with Retrying {
  private val logger = LoggerFactory.getLogger(getClass)
  def callback: Action[CallBackNotification] = Action.async(parse.json[CallBackNotification]) { implicit request =>
    val notification: CallBackNotification = request.body
    logger.info(
      s"SDES: Received callback for fileName: ${notification.filename}, correlationId: ${notification.correlationID}, status: ${fromName(
        notification.notification
      )} and possible failedReason: ${notification.failureReason.getOrElse("")}"
    )

    retry(
      sdesAlgebra.update(notification),
      List(100.milliseconds, 1.seconds, 2.seconds),
      s"SDES correlation id : ${notification.correlationID}"
    ).map(_ => Ok)
  }
}
