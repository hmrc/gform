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

package uk.gov.hmrc.gform.obligation

import uk.gov.hmrc.gform.des.{ DesConnector, TaxPeriodDes }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.gform.sharedmodel.TaxPeriod
import uk.gov.hmrc.gform.sharedmodel.formtemplate.HmrcTaxPeriod

import scala.concurrent.ExecutionContext
class ObligationService(desConnector: DesConnector) {

  def callDES(hmrcTaxPeriod: HmrcTaxPeriod)(implicit hc: HeaderCarrier, ex: ExecutionContext) =
    desConnector.lookupTaxPeriod(hmrcTaxPeriod.idType, hmrcTaxPeriod.idNumber, hmrcTaxPeriod.regimeType)

}
