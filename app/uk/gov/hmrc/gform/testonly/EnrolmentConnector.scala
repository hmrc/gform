/*
 * Copyright 2023 HM Revenue & Customs
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

import play.api.libs.json.JsValue
import uk.gov.hmrc.http._
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw
import uk.gov.hmrc.http.client.HttpClientV2

import scala.concurrent.{ ExecutionContext, Future }

class EnrolmentConnector(httpClient: HttpClientV2, baseUrl: String)(implicit ec: ExecutionContext) {

  def upload(id: String, body: JsValue)(implicit hc: HeaderCarrier) =
    httpClient
      .put(url"$ES6url/HMRC-OBTDS-ORG~EtmpRegistrationNumber~$id")
      .withBody(body)
      .execute[HttpResponse]

  def deEnrol(userId: String, id: String)(implicit hc: HeaderCarrier) =
    httpClient
      .delete(url"$baseUrl/enrolment-store/users/$userId/enrolments/HMRC-OBTDS-ORG~EtmpRegistrationNumber~$id")
      .execute[HttpResponse]

  def removeUnallocated(id: String)(implicit hc: HeaderCarrier): Future[Unit] =
    httpClient
      .delete(url"$ES6url/HMRC-OBTDS-ORG~EtmpRegistrationNumber~$id")
      .execute[HttpResponse]
      .map(_ => ())

  private val ES6url = "http://enrolment-store-proxy.protected.mdtp:80/enrolment-store-proxy/enrolment-store/enrolments"

}
