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

import java.text.SimpleDateFormat

import cats.data.NonEmptyList
import play.api.Logger
import play.api.mvc.Action
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.des._
import uk.gov.hmrc.gform.sharedmodel.{ HmrcTaxPeriodWithEvaluatedId, TaxPeriodIdentifier, TaxResponse }

import scala.concurrent.Future

class ObligationController(obligation: ObligationService) extends BaseController {

  def getAllTaxPeriods() = Action.async(parse.json[List[HmrcTaxPeriodWithEvaluatedId]]) { implicit request =>
    Logger.info(s"Get All Tax Periods from DES, ${loggingHelpers.cleanHeaders(request.headers)}")
    val body: List[HmrcTaxPeriodWithEvaluatedId] = request.body
    val b = body.map(i => {
      obligation
        .callDES(i.hmrcTaxPeriod.idType.value, i.idNumberValue.value, i.hmrcTaxPeriod.regimeType.value)
        .map(x => TaxResponse(i.hmrcTaxPeriod, x))
    })
    Future.sequence(b).asOkJson
  }
}
