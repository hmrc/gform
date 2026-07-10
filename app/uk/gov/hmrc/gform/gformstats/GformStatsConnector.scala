/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.gformstats

import org.slf4j.LoggerFactory
import play.api.http.Status.INTERNAL_SERVER_ERROR
import play.api.libs.json.Json
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse, StringContextOps }
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.client.HttpClientV2

import scala.concurrent.ExecutionContext

class GformStatsConnector(httpClient: HttpClientV2, baseUrl: String, enabled: Boolean)(implicit ec: ExecutionContext) {

  private val logger = LoggerFactory.getLogger(getClass)

  def sendEvent(
    formTemplateId: String,
    formTemplateName: String,
    eventType: String,
    destinations: Option[List[String]]
  )(implicit hc: HeaderCarrier): Unit =
    if (enabled) {
      val payload = Json.obj(
        "formTemplateId"   -> formTemplateId,
        "formTemplateName" -> formTemplateName,
        "eventType"        -> eventType,
        "destinations"     -> destinations
      )

      httpClient
        .post(url"$baseUrl/gform-stats/events")
        .withBody(payload)
        .execute[HttpResponse]
        .recover { case ex: Exception =>
          logger.error(s"Failed to send $eventType event for form template $formTemplateId to gform-stats", ex)
          HttpResponse(INTERNAL_SERVER_ERROR, "")
        }

      ()
    }
}
