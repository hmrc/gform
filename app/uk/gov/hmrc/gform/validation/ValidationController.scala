/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.validation

import org.slf4j.LoggerFactory
import play.api.libs.json.Json
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.des.DesRegistrationRequest

import scala.concurrent.{ ExecutionContext, Future }

class ValidationController(controllerComponents: ControllerComponents, validation: ValidationService[Future])(implicit
  ex: ExecutionContext
) extends BaseController(controllerComponents) {

  private val logger = LoggerFactory.getLogger(getClass)

  def desRegistration(utr: String) = Action.async(parse.json[DesRegistrationRequest]) { implicit request =>
    logger.info(s"validate Address At Des, ${loggingHelpers.cleanHeaders(request.headers)}")
    validation.desRegistration(utr, request.body).map(a => Ok(Json.toJson(a)))
  }
}
