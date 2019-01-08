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

import play.api.Logger
import play.api.mvc.Action
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.des.TaxPeriodDes
import uk.gov.hmrc.gform.sharedmodel.{ TaxPeriod, TaxPeriods }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.HmrcTaxPeriod
import scala.concurrent.duration._
import play.api.libs.json.{ JsValue, Json, OFormat }

class ObligationController(obligation: ObligationService) extends BaseController {

  def getTaxPeriods(idType: String, idNumber: String, regimeType: String) = Action.async(parse.json[HmrcTaxPeriod]) {
    implicit request =>
      Logger.info(s"Get Tax Periods from DES, ${loggingHelpers.cleanHeaders(request.headers)}")
      obligation
        .callDES(request.body)
        .map(
          i =>
            Ok(
              Json.obj("obligations" -> new TaxPeriods(i.obligationDetails.map(
                j => new TaxPeriod(j.inboundCorrespondenceFromDate, j.inboundCorrespondenceToDate, j.periodKey)
              )))))
  }
}
