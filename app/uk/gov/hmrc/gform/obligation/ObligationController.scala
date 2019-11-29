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

import cats.data.NonEmptyList
import play.api.Logger
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.{ HmrcTaxPeriodWithEvaluatedId, TaxResponse }

import scala.concurrent.{ ExecutionContext, Future }

class ObligationController(controllerComponents: ControllerComponents, obligation: ObligationService[Future])(
  implicit ex: ExecutionContext)
    extends BaseController(controllerComponents) {

  def getAllTaxPeriods() = Action.async(parse.json[NonEmptyList[HmrcTaxPeriodWithEvaluatedId]]) { implicit request =>
    Logger.info(s"Get All Tax Periods from DES, ${loggingHelpers.cleanHeaders(request.headers)}")
    val body: NonEmptyList[HmrcTaxPeriodWithEvaluatedId] = request.body
    val b = body.map { i =>
      val hmrcTaxPeriod = i.recalculatedTaxPeriodKey.hmrcTaxPeriod
      obligation
        .callDES(hmrcTaxPeriod.idType.value, i.idNumberValue.value, hmrcTaxPeriod.regimeType.value)
        .map(x => TaxResponse(i, x))
    }
    Future.sequence(b.toList).asOkJson
  }
}
