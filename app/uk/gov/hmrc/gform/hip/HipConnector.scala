/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.hip

import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.config.HipConnectorConfig
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReadsEither, HttpReadsHttpResponse, LowPriorityHttpReadsJson }
import play.api.http.HeaderNames.AUTHORIZATION
import play.api.libs.json.JsArray
import uk.gov.hmrc.gform.sharedmodel.hip.HipEmploymentSummary

import java.util.Base64
import scala.concurrent.{ ExecutionContext, Future }

trait HipAlgebra[F[_]] {

  def lookupEmployment(
    nino: String,
    taxYear: Int
  ): F[JsArray]
}

class HipConnector(wSHttp: WSHttp, baseUrl: String, hipConfig: HipConnectorConfig)(implicit ec: ExecutionContext)
    extends HipAlgebra[Future] with LowPriorityHttpReadsJson with HttpReadsEither with HttpReadsHttpResponse {

  private val logger = LoggerFactory.getLogger(getClass)

  private implicit val hc: HeaderCarrier = HeaderCarrier(
    extraHeaders = Seq(
      "correlationId"        -> hipConfig.correlationId,
      "gov-uk-originator-id" -> hipConfig.originatorId
    )
  )

  private val authHeaders: Seq[(String, String)] = Seq(
    AUTHORIZATION -> s"Basic ${Base64.getEncoder
      .encodeToString(s"${hipConfig.clientId}:${hipConfig.clientSecret}".getBytes("UTF-8"))}"
  )

  def lookupEmployment(
    nino: String,
    taxYear: Int
  ): Future[JsArray] = {
    logger.info(
      s"Hip employments called, ${loggingHelpers.cleanHeaderCarrierHeader(hc)}"
    )

    val url =
      s"$baseUrl${hipConfig.basePath}/nps/nps-json-service/nps/v1/api/employment/employment-summary/$nino/taxYear/$taxYear"

    logger.info(s"calling $url with authHeaders: ${authHeaders.toList}")

    wSHttp
      .GET[HipEmploymentSummary](url, headers = authHeaders)
      .map(_.individualsEmploymentDetails)
  }
}
