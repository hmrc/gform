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

package uk.gov.hmrc.gform.des

import org.slf4j.LoggerFactory
import play.api.libs.json.Json
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.des.DesRegistrationRequest

import scala.concurrent.{ ExecutionContext, Future }

class DesController(controllerComponents: ControllerComponents, desService: DesService[Future])(implicit
  ex: ExecutionContext
) extends BaseController(controllerComponents) {

  private val logger = LoggerFactory.getLogger(getClass)

  def organisation(utr: String) = Action.async(parse.json[DesRegistrationRequest]) { implicit request =>
    logger.info(s"Get Address from DES, ${loggingHelpers.cleanHeaders(request.headers)}")
    desService.organisation(utr, request.body).map(a => Ok(Json.toJson(a)))
  }

  def agentDetails(idType: String, idNumber: String) = Action.async { implicit request =>
    logger.info(s"Get Agent Details from DES, ${loggingHelpers.cleanHeaders(request.headers)}")
    desService.agentDetails(idType, idNumber).map(a => Ok(Json.toJson(a)))
  }
}
