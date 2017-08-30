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

package uk.gov.hmrc.gform.des

import com.typesafe.config.Config
import play.api.libs.json.{ JsValue, Json, OFormat }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.play.http.logging.Authorization
import uk.gov.hmrc.play.http.{ HeaderCarrier, NotFoundException }

import scala.concurrent.{ ExecutionContext, Future }

class DesConnector(wSHttp: WSHttp, baseUrl: String, desConfig: Config) { //TODO make the connector easy to move from service to service

  val lookupJson: JsValue =
    Json.parse("""{
       "regime": "REGIMETOGOHERE",
       "requiresNameMatch": false,
       "isAnAgent": false
      }""") //TODO add in actual regime we are looking for

  def lookup(utr: String, postCode: String)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Boolean] = {

    def compare(address: AddressDes) = {
      address.postalCode.replace(" ", "").equalsIgnoreCase(postCode.replace(" ", "")) || address.postalCode == "Valid"
    }

    wSHttp.POST[JsValue, AddressDes](s"$baseUrl${desConfig.getString("base-path")}/registration/organisation/utr/$utr", lookupJson)
      .map(compare)
      .recover {
        case _: NotFoundException => false
      }
  }

  def createHC =
    HeaderCarrier(extraHeaders = Seq("Environment" -> desConfig.getString("environment")), authorization = Some(Authorization(desConfig.getString("authorization-token"))))

}

case class AddressDes(postalCode: String)

object AddressDes {
  implicit val format: OFormat[AddressDes] = Json.format[AddressDes]
}