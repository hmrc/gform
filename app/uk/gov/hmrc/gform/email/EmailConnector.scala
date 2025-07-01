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

package uk.gov.hmrc.gform.email

import org.slf4j.LoggerFactory
import play.api.libs.json.Json
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.http._
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw
import uk.gov.hmrc.http.client.HttpClientV2

import scala.concurrent.{ ExecutionContext, Future }

class EmailConnector(httpClient: HttpClientV2, baseUrl: String)(implicit ec: ExecutionContext) {

  private val logger = LoggerFactory.getLogger(getClass)

  def sendEmail(emailTemplate: EmailTemplate)(implicit headerCarrier: HeaderCarrier): Future[Unit] = {
    logger.info(s"send email, ${loggingHelpers.cleanHeaderCarrierHeader(headerCarrier)}")
    httpClient
      .post(url"$baseUrl/hmrc/email")
      .withBody(Json.toJson(emailTemplate))
      .execute[HttpResponse]
      .map(_ => ())
  }
}
