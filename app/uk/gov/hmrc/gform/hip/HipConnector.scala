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
import uk.gov.hmrc.http.{ BadRequestException, ForbiddenException, HeaderCarrier, HttpReadsEither, HttpReadsHttpResponse, HttpResponse, InternalServerException, LowPriorityHttpReadsJson, NotFoundException, ServiceUnavailableException, StringContextOps, UnauthorizedException }
import play.api.http.HeaderNames.AUTHORIZATION
import uk.gov.hmrc.http.client.HttpClientV2

import scala.concurrent.{ ExecutionContext, Future }

trait HipAlgebra[F[_]] {

  def getPegaCaseActionDetails(
    caseId: String,
    actionId: String
  ): F[String]
  def pegaChangeToNextStage(
    caseId: String,
    eTag: String
  ): F[String]
}

class HipConnector(http: HttpClientV2, baseUrl: String, hipConfig: HipConnectorConfig)(implicit ec: ExecutionContext)
    extends HipAlgebra[Future] with LowPriorityHttpReadsJson with HttpReadsEither with HttpReadsHttpResponse {

  private val logger = LoggerFactory.getLogger(getClass)

  private implicit val hc: HeaderCarrier = HeaderCarrier(
    extraHeaders = Seq(
      "correlationId"        -> hipConfig.correlationId,
      "gov-uk-originator-id" -> hipConfig.originatorId
    )
  )

  private def authorization: String =
    s"Basic ${hipConfig.authorizationToken}"

  private val authHeaders: Seq[(String, String)] = Seq(
    AUTHORIZATION -> s"$authorization"
  )

  def getPegaCaseActionDetails(caseId: String, actionId: String): Future[String] = {
    logger.info(
      s"getPegaCaseActionDetails called, ${loggingHelpers.cleanHeaderCarrierHeader(hc)}"
    )
    val url =
      s"$baseUrl${hipConfig.basePath}/pega/prweb/api/application/v2/cases/$caseId/actions/$actionId"

    http
      .get(url"$url")
      .setHeader(authHeaders: _*)
      .setHeader(("x-origin-channel", "Web"))
      .execute[HttpResponse]
      .map(r => handlePegaResponse(r, caseId))
  }

  def pegaChangeToNextStage(caseId: String, eTag: String): Future[String] = {
    logger.info(
      s"pegaChangeToNextStage called, ${loggingHelpers.cleanHeaderCarrierHeader(hc)}"
    )

    val url =
      s"$baseUrl${hipConfig.basePath}/pega/prweb/api/application/v2/cases/$caseId/stages/next?viewType=none&cleanupProcesses=false"

    http
      .post(url"$url")
      .setHeader(authHeaders: _*)
      .setHeader(("x-origin-channel", "Web"))
      .setHeader(("if-match", eTag))
      .execute[HttpResponse]
      .map(r => handlePegaResponse(r, caseId))
  }

  private def handlePegaResponse(response: HttpResponse, caseId: String): String =
    response.status match {
      case 200 =>
        response
          .header("etag") match {
          case Some(eTag) => eTag
          case None =>
            throw new InternalServerException(s"etag not found in Pega response for Case ID: $caseId")
        }
      case 400 =>
        logger.error(
          s"Received bad request response from Pega API: ${response.body}"
        )
        throw new BadRequestException(s"Bad request response from Pega API for Case ID: $caseId")
      case 401 =>
        logger.error(
          s"Received unauthorized response from Pega API: ${response.body}"
        )
        throw new UnauthorizedException("Unauthorized request to Pega API")
      case 403 =>
        logger.error(
          s"Received forbidden response from Pega API: ${response.body}"
        )
        throw new ForbiddenException("Forbidden request to Pega API")
      case 404 =>
        throw new NotFoundException(s"Pega API returned Case ID: $caseId not found")
      case 500 =>
        logger.error(
          s"Received internal server error response from Pega API: ${response.body}"
        )
        throw new InternalServerException("Internal server error response from Pega API")
      case 503 =>
        val message = "Received service unavailable response from Pega API"
        logger.error(message)
        throw new ServiceUnavailableException(message)
      case status =>
        logger.error(s"Received unexpected status $status from Pega API. ${response.body}")
        throw new InternalServerException("Unexpected response code from Pega API")
    }

}
