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

package uk.gov.hmrc.gform.validation

import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc.Action
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.bank_account_reputation.{ BankAccountReputationConnector, Response }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.des.{ AddressDes, DesConnector }
import uk.gov.hmrc.gform.sharedmodel.{ Account, ValAddress }

import scala.concurrent.Future

class ValidationController(validation: ValidationService) extends BaseController {

  def validateAddressAtDes() = Action.async(parse.json[ValAddress]) { implicit request =>
    Logger.info(s"validate Address At Des, ${loggingHelpers.cleanHeaders(request.headers)}")
    validation.callDes(request.body).map(if (_) NoContent else NotFound)
  }

  def validateBank() = Action.async(parse.json[Account]) { implicit request =>
    Logger.info(s"validate bank, ${loggingHelpers.cleanHeaders(request.headers)}'")
    validation.callBRS(request.body).map(if (_) NoContent else NotFound)
  }
}
