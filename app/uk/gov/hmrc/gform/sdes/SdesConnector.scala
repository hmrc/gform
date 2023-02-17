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

package uk.gov.hmrc.gform.sdes

import org.slf4j.LoggerFactory
import play.api.libs.json.Json
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesNotifyRequest
import uk.gov.hmrc.gform.wshttp.{ FutureHttpResponseSyntax, WSHttp }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads, HttpReadsInstances, HttpResponse }

import scala.concurrent.{ ExecutionContext, Future }

class SdesConnector(wSHttp: WSHttp, sdesBaseUrl: String, sdesBasePath: String, headers: Seq[(String, String)])(implicit
  ex: ExecutionContext
) {
  private val logger = LoggerFactory.getLogger(getClass)

  implicit val legacyRawReads: HttpReads[HttpResponse] =
    HttpReadsInstances.throwOnFailure(HttpReadsInstances.readEitherOf(HttpReadsInstances.readRaw))

  def notifySDES(payload: SdesNotifyRequest)(implicit hc: HeaderCarrier): Future[HttpResponse] = {
    logger.debug(s"SDES notification request: ${Json.stringify(Json.toJson(payload))}")
    val url = s"$sdesBaseUrl$sdesBasePath/notification/fileready"
    wSHttp
      .POST[SdesNotifyRequest, HttpResponse](url, payload, headers)
      .failWithNonSuccessStatusCodes(url)
  }
}
