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

import org.apache.pekko.actor.Scheduler
import org.slf4j.LoggerFactory
import play.api.mvc.{ Action, ControllerComponents }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.sdes.CallBackNotification

import scala.concurrent.{ ExecutionContext, Future }

class SdesCallbackController(
  cc: ControllerComponents,
  sdesAlgebra: SdesAlgebra[Future]
)(implicit ex: ExecutionContext, schduler: Scheduler)
    extends BaseController(cc) {
  private val logger = LoggerFactory.getLogger(getClass)
  def callback: Action[CallBackNotification] = Action.async(parse.json[CallBackNotification]) { implicit request =>
    val notification: CallBackNotification = request.body
    val commonMsgPart =
      s"${notification.notification} callback for fileName: ${notification.filename}, correlationId: ${notification.correlationID} and possible failedReason: ${notification.failureReason
        .getOrElse("")}"
    logger.info(s"SDES: Received $commonMsgPart")

    sdesAlgebra.update(notification).map {
      case Right(_) =>
        logger.info(s"SDES: Successfully updated $commonMsgPart")
        Ok
      case Left(unexpected) =>
        logger.error(
          s"SDES: Failed to update $commonMsgPart with error: ${unexpected.error}",
          new RuntimeException(unexpected.error)
        )
        Ok
    }
  }
}
