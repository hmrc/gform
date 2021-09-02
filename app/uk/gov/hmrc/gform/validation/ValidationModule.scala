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

import cats.instances.future._
import uk.gov.hmrc.gform.bank_account_reputation.BankAccountReputationConnector
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.des.{ DesAlgebra, DesConnector }
import uk.gov.hmrc.gform.wshttp.WSHttpModule

import scala.concurrent.{ ExecutionContext, Future }

class ValidationModule(wSHttpModule: WSHttpModule, configModule: ConfigModule)(implicit ex: ExecutionContext) {

  private val desConfig = configModule.desConfig
  private val desConnector: DesAlgebra[Future] =
    new DesConnector(
      wSHttpModule.auditableWSHttp,
      configModule.serviceConfig.baseUrl("etmp-hod"),
      desConfig
    )

  private val bankAccountReputationConnector =
    new BankAccountReputationConnector(
      wSHttpModule.auditableWSHttp,
      configModule.serviceConfig.baseUrl("bank-account-reputation")
    )

  private val validationService = new ValidationService(desConnector, bankAccountReputationConnector)
  val validationController = new ValidationController(configModule.controllerComponents, validationService)
}
