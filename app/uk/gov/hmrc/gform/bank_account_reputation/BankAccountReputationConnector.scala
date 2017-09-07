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

package uk.gov.hmrc.gform.bank_account_reputation

import play.api.libs.json._
import uk.gov.hmrc.gform.config.DesConnectorConfig
import uk.gov.hmrc.gform.wshttp.WSHttp
import play.api.libs.functional.syntax._
import uk.gov.hmrc.play.http.HeaderCarrier
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class BankAccountReputationConnector(wSHttp: WSHttp, baseUrl: String) {

  def exists(accountNumber: String, sortCode: String)(implicit hc: HeaderCarrier): Future[Response] =
    wSHttp.POST[Account, Response](s"$baseUrl/modcheck", Account(sortCode, accountNumber))

}

case class Account(
  sortCode: String,
  accountNumber: String
)

object Account {
  implicit val format: OFormat[Account] = Json.format[Account]
}

case class Response(
  accountNumberWithSortCodeIsValid: Boolean,
  nonStandardAccountDetailsRequiredForBacs: String
)

object Response {
  private val reads = Reads[Response] { json =>
    (json \ "parameters" \ "nonStandardAccountDetailsRequiredForBacs").asOpt[String] match {
      case Some(str) => parse(json, str)
      case None => JsError("the response does not match desired parameters : accountNumberWithSortCodeIsValid, accountNumberWithSortCodeIsValid")
    }
  }

  private def parse(json: JsValue, str: String) =
    str match {
      case "no" | "yes" | "inapplicable" => JsSuccess(Response((json \ "accountNumberWithSortCodeIsValid").as[Boolean], str))
      case _ => JsError("Response did not match no, yes, inapplicable")
    }

  private val basic: OFormat[Response] = Json.format[Response]

  private val readsAll = (basic: Reads[Response]) | reads
  private val writes: OWrites[Response] = basic

  implicit val format: OFormat[Response] = OFormat(readsAll, writes)
}
