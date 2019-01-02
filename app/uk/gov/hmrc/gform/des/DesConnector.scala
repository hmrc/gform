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

package uk.gov.hmrc.gform.des

import com.typesafe.config.Config
import play.api.Logger
import play.api.libs.json.{ JsValue, Json, OFormat }
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.config.DesConnectorConfig
import uk.gov.hmrc.gform.wshttp.WSHttp

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.Authorization

class DesConnector(wSHttp: WSHttp, baseUrl: String, desConfig: DesConnectorConfig) {

  val lookupJson: JsValue =
    Json.parse("""{
       "regime": "REGIMETOGOHERE",
       "requiresNameMatch": false,
       "isAnAgent": false
      }""") //TODO add in actual regime we are looking for

  def lookup(utr: String)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[AddressDes] = {
    implicit val hc = HeaderCarrier(
      extraHeaders = Seq("Environment" -> desConfig.environment),
      authorization = Some(Authorization(desConfig.authorizationToken)))
    Logger.info(s"Des lookup, UTR: '$utr', ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")
    wSHttp.POST[JsValue, AddressDes](s"$baseUrl${desConfig.basePath}/registration/organisation/utr/$utr", lookupJson)
  }

}

case class AddressDes(postalCode: String)

object AddressDes {
  implicit val format: OFormat[AddressDes] = Json.format[AddressDes]
}
