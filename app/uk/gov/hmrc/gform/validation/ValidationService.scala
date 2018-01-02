/*
 * Copyright 2018 HM Revenue & Customs
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

import uk.gov.hmrc.gform.bank_account_reputation.BankAccountReputationConnector
import uk.gov.hmrc.gform.des.{ AddressDes, DesConnector }
import uk.gov.hmrc.gform.sharedmodel.{ Account, ValAddress }

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.{ HeaderCarrier, NotFoundException }

class ValidationService(desConnector: DesConnector, bankAccountReputationConnector: BankAccountReputationConnector) {

  def callDes(valAddress: ValAddress)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Boolean] = {
    def compare(address: AddressDes) = {
      address.postalCode.replace(" ", "").equalsIgnoreCase(valAddress.postCode.replace(" ", ""))
    }
    desConnector.lookup(valAddress.utr).map(compare)
      .recover {
        case _: NotFoundException => false
      }
  }

  def callBRS(account: Account)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Boolean] =
    bankAccountReputationConnector.exists(account).map(_.accountNumberWithSortCodeIsValid)

}
