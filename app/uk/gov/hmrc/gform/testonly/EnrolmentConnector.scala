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

package uk.gov.hmrc.gform.testonly

import play.api.libs.json.{ JsValue, Json }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.play.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.{ ExecutionContext, Future }

class EnrolmentConnector(wSHttp: WSHttp, baseUrl: String) {

  def upload(country: String)(implicit hc: HeaderCarrier, executionContext: ExecutionContext) = {
    val ref = getRef(country)
    wSHttp.PUT[JsValue, HttpResponse](s"$ES6url/HMRC-OBTDS-ORG~EtmpRegistrationNumber~$ref", getJson(country))
  }

  def deEnrol(userId: String, country: String)(implicit hc: HeaderCarrier) = {
    val ref = getRef(country)
    wSHttp.DELETE(s"$baseUrl/enrolment-store/users/$userId/enrolments/HMRC-OBTDS-ORG~EtmpRegistrationNumber~$ref")
  }

  def removeUnallocated(country: String)(implicit hc: HeaderCarrier, executionContext: ExecutionContext): Future[Unit] = {
    val ref = getRef(country)
    wSHttp.DELETE[HttpResponse](s"$ES6url/HMRC-OBTDS-ORG~EtmpRegistrationNumber~").map(_ => ())
  }

  private def getRef(country: String) = if (country == "uk") "WLAS50703269741" else "WLAS50703269741"

  private def getJson(isUk: String): JsValue = {
    if (isUk == "uk") {
      Json.parse(
        s"""{"verifiers" : [{"key" : "NonUkCountryCode","value" : "GB"},{"key" : "BusinessPostcode","value" : "E499OL"}]}"""
      )
    } else {
      Json.parse(
        s"""{"verifiers" : [{"key" : "NonUkCountryCode","value" : "BV"}]}"""
      )
    }
  }

  private val ES6url = "http://enrolment-store-proxy.protected.mdtp:80/enrolment-store-proxy/enrolment-store/enrolments"

}
