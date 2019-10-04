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

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.bank_account_reputation.BankAccountReputationConnector
import uk.gov.hmrc.gform.des.{ AddressDes, DesConnector }
import uk.gov.hmrc.gform.sharedmodel.ServiceCallResponse
import uk.gov.hmrc.gform.sharedmodel.des.{ DesRegistrationRequest, DesRegistrationResponse }
import uk.gov.hmrc.gform.sharedmodel.Account
import uk.gov.hmrc.http.HeaderCarrier

class ValidationService(desConnector: DesConnector, bankAccountReputationConnector: BankAccountReputationConnector)(
  implicit ec: ExecutionContext) {

  def desRegistration(
    utr: String,
    desRegistrationRequest: DesRegistrationRequest): Future[ServiceCallResponse[DesRegistrationResponse]] =
    desConnector.lookupRegistration(utr, desRegistrationRequest)

  def callBRS(account: Account)(implicit hc: HeaderCarrier): Future[Boolean] =
    bankAccountReputationConnector.exists(account).map(_.accountNumberWithSortCodeIsValid)

}
