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

  def upload(id: String, body: JsValue)(implicit hc: HeaderCarrier, executionContext: ExecutionContext) = {
    wSHttp.PUT[JsValue, HttpResponse](s"$ES6url/HMRC-OBTDS-ORG~EtmpRegistrationNumber~$id", body)
  }

  def deEnrol(userId: String, id: String)(implicit hc: HeaderCarrier) = {
    wSHttp.DELETE(s"$baseUrl/enrolment-store/users/$userId/enrolments/HMRC-OBTDS-ORG~EtmpRegistrationNumber~$id")
  }

  def removeUnallocated(id: String)(implicit hc: HeaderCarrier, executionContext: ExecutionContext): Future[Unit] = {
    wSHttp.DELETE[HttpResponse](s"$ES6url/HMRC-OBTDS-ORG~EtmpRegistrationNumber~$id").map(_ => ())
  }

  private val ES6url = "http://enrolment-store-proxy.protected.mdtp:80/enrolment-store-proxy/enrolment-store/enrolments"

}
