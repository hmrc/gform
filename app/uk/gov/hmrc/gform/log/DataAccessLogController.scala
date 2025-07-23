/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.log

import play.api.libs.json.Json
import play.api.mvc.{ Action, AnyContent, ControllerComponents }
import uk.gov.hmrc.gform.controllers.BaseController

import scala.concurrent.{ ExecutionContext, Future }

class DataAccessLogController(
  cc: ControllerComponents,
  dataAccessService: DataAccessAlgebra[Future]
)(implicit ex: ExecutionContext)
    extends BaseController(cc) {

  def getDataAccessLogs(page: Int, pageSize: Int): Action[AnyContent] =
    Action.async { _ =>
      dataAccessService
        .page(page, pageSize)
        .map(logs => Ok(Json.toJson(logs)))
    }

  def saveDataAccessLog(): Action[DataAccessLog] = Action.async(parse.json[DataAccessLog]) { request =>
    val log: DataAccessLog = request.body
    dataAccessService
      .save(log)
      .map { _ =>
        NoContent
      }
  }
}
