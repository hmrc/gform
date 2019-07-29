/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.submission

import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc.{ Action, AnyContent }
import uk.gov.hmrc.gform.controllers.BaseController

import scala.concurrent.ExecutionContext.Implicits.global

class DestinationAuditController(destinationAuditService: DestinationAuditService) extends BaseController {

  def getLatestEventWithReturnedStatus(submissionRef: String): Action[AnyContent] = Action.async { implicit request =>
    Logger.info(
      s"Getting latest destination audit event with status Returned for the following submission ref: $submissionRef")
    destinationAuditService.findLatestDestinationAuditWithEventReturnedStatus(submissionRef).map {
      case Some(destinationAudit) => Ok(Json.toJson(destinationAudit))
      case None =>
        BadRequest(s"Failed to find a destination audit event with status Returned and submission ref: $submissionRef")
    }
  }
}
