/*
 * Copyright 2022 HM Revenue & Customs
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

import org.slf4j.LoggerFactory
import play.api.mvc.{ Action, ControllerComponents }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.sdes.CallBackNotification

import scala.concurrent.{ ExecutionContext, Future }

class SdesCallbackController(cc: ControllerComponents)(implicit ex: ExecutionContext) extends BaseController(cc) {
  private val logger = LoggerFactory.getLogger(getClass)

  def callback: Action[CallBackNotification] = Action.async(parse.json[CallBackNotification]) { request =>
    val CallBackNotification(status, fileName, correlationID, maybeFailureReason) = request.body
    logger.info(
      s"Received SDES callback for fileName: $fileName, correlationId : $correlationID, status : $status and failedReason: ${maybeFailureReason
        .getOrElse("")} "
    )
    Future.successful(Ok)
  }
}
