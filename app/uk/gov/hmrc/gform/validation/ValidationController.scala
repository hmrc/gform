/*
 * Copyright 2017 HM Revenue & Customs
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
import uk.gov.hmrc.gform.bank_account_reputation.{ Account, BankAccountReputationConnector, Response }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.des.{ AddressDes, DesConnector }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ValidationController(desConnector: DesConnector, bankAccountReputationConnector: BankAccountReputationConnector) extends BaseController {

  def validateAddressAtDes(utr: String, postcode: String) = Action.async { implicit request =>
    desConnector.lookup(utr, postcode).map(if (_) NoContent else NotFound)
  }

  def validateBank(accountNumber: String, sortCode: String) = Action.async { implicit request =>
    bankAccountReputationConnector.exists(accountNumber, sortCode).map(if (_) NoContent else NotFound)
  }

  def testValidatorStub(utr: String) = Action.async { implicit request =>
    if (utr.startsWith("1")) {
      Future.successful(Ok(Json.toJson(AddressDes("Valid"))))
    } else
      Future.successful(Ok(Json.toJson(AddressDes("Fail"))))
  }
}
