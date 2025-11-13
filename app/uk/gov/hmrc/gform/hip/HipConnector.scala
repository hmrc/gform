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
import play.api.http.Status._
import play.api.libs.json.{ JsObject, JsValue, Json }
import uk.gov.hmrc.gform.sharedmodel.sdes.CorrelationId
import uk.gov.hmrc.http.client.HttpClientV2

import scala.concurrent.{ ExecutionContext, Future }

trait HipAlgebra[F[_]] {

  def getPegaCaseActionDetails(
    caseId: String,
    actionId: String,
    correlationId: String
  ): F[String]
  def pegaChangeToNextStage(
    caseId: String,
    eTag: String,
    correlationId: String
  ): F[String]
  def validateNIClaimReference(
    nino: String,
    claimReference: String,
    correlationId: CorrelationId
  ): F[JsValue]
  def niClaimUpdateBankDetails(
    nino: String,
    bankAccountName: String,
    sortCode: String,
    accountNumber: String,
    rollNumber: Option[String],
    refundClaimReference: String,
    correlationId: String
  ): F[JsValue]
}

class HipConnector(http: HttpClientV2, baseUrl: String, hipConfig: HipConnectorConfig)(implicit ec: ExecutionContext)
    extends HipAlgebra[Future] with LowPriorityHttpReadsJson with HttpReadsEither with HttpReadsHttpResponse {

  private val logger = LoggerFactory.getLogger(getClass)

  private object Headers {
    val CorrelationId = "correlationId"
    val GovUkOriginatorId = "gov-uk-originator-id"
    val OriginChannel = "x-origin-channel"
    val IfMatch = "if-match"
  }

  private object Values {
    val WebChannel = "Web"
  }

  private implicit val hc: HeaderCarrier = HeaderCarrier(
    extraHeaders = Seq(
      Headers.GovUkOriginatorId -> hipConfig.originatorId
    )
  )

  private def authorization: String =
    s"Basic ${hipConfig.authorizationToken}"

  private val authHeaders: Seq[(String, String)] = Seq(
    AUTHORIZATION -> authorization
  )

  private def buildPegaUrl(path: String): String =
    s"$baseUrl${hipConfig.basePath}/pega/prweb/api/application/v2/$path"

  private def buildNiUrl(path: String): String =
    s"$baseUrl${hipConfig.basePath}/ni/$path"

  private def handleResponse[T](
    response: HttpResponse,
    apiName: String,
    identifier: String,
    successHandler: HttpResponse => T
  ): T =
    response.status match {
      case OK => successHandler(response)
      case BAD_REQUEST =>
        logger.error(s"Received bad request response from $apiName: ${response.body}")
        throw new BadRequestException(s"Bad request response from $apiName for identifier: $identifier")
      case UNAUTHORIZED =>
        logger.error(s"Received unauthorized response from $apiName: ${response.body}")
        throw new UnauthorizedException(s"Unauthorized request to $apiName")
      case FORBIDDEN =>
        logger.error(s"Received forbidden response from $apiName: ${response.body}")
        throw new ForbiddenException(s"Forbidden request to $apiName")
      case NOT_FOUND =>
        throw new NotFoundException(s"$apiName returned identifier: $identifier not found")
      case INTERNAL_SERVER_ERROR =>
        logger.error(s"Received internal server error response from $apiName: ${response.body}")
        throw new InternalServerException(s"Internal server error response from $apiName")
      case SERVICE_UNAVAILABLE =>
        val message = s"Received service unavailable response from $apiName"
        logger.error(message)
        throw new ServiceUnavailableException(message)
      case status =>
        logger.error(s"Received unexpected status $status from $apiName. ${response.body}")
        throw new InternalServerException(s"Unexpected response code from $apiName")
    }

  def getPegaCaseActionDetails(caseId: String, actionId: String, correlationId: String): Future[String] = {
    logger.info(s"getPegaCaseActionDetails called, ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")

    val url = buildPegaUrl(s"cases/$caseId/actions/$actionId")

    http
      .get(url"$url")
      .setHeader(authHeaders: _*)
      .setHeader((Headers.OriginChannel, Values.WebChannel))
      .setHeader(Headers.CorrelationId -> correlationId)
      .execute[HttpResponse]
      .map(response => handleResponse(response, "Pega API", caseId, extractEtag(_, caseId)))
  }

  def pegaChangeToNextStage(caseId: String, eTag: String, correlationId: String): Future[String] = {
    logger.info(s"pegaChangeToNextStage called, ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")

    val url = buildPegaUrl(s"cases/$caseId/stages/next?viewType=none&cleanupProcesses=false")

    http
      .post(url"$url")
      .setHeader(authHeaders: _*)
      .setHeader((Headers.OriginChannel, Values.WebChannel))
      .setHeader(Headers.CorrelationId -> correlationId)
      .setHeader((Headers.IfMatch, eTag))
      .execute[HttpResponse]
      .map(response => handleResponse(response, "Pega API", caseId, extractEtag(_, caseId)))
  }

  def validateNIClaimReference(
    nino: String,
    claimReference: String,
    correlationId: CorrelationId
  ): Future[JsValue] = {
    logger.info(
      s"validateNIClaimReference called for reference '$claimReference', ${loggingHelpers.cleanHeaderCarrierHeader(hc)}"
    )

    val url = buildNiUrl(s"person/$nino/national-insurance/claim/refund/$claimReference")

    http
      .get(url"$url")
      .setHeader(authHeaders: _*)
      .setHeader(Headers.CorrelationId -> correlationId.value)
      .execute[HttpResponse]
      .map(response => handleResponse(response, "Validate NI Claim Reference", claimReference, _.json))
  }

  def niClaimUpdateBankDetails(
    nino: String,
    bankAccountName: String,
    sortCode: String,
    accountNumber: String,
    rollNumber: Option[String],
    refundClaimReference: String,
    correlationId: String
  ): Future[JsValue] = {
    logger.info(
      s"niClaimUpdateBankDetails called for reference '$refundClaimReference', ${loggingHelpers.cleanHeaderCarrierHeader(hc)}"
    )

    val url = buildNiUrl(s"contributions/$nino/claim/refund/$refundClaimReference/bank-details")
    val body = buildBankDetailsBody(bankAccountName, sortCode, accountNumber, rollNumber)

    http
      .put(url"$url")
      .setHeader(authHeaders: _*)
      .setHeader(Headers.CorrelationId -> correlationId)
      .withBody(body)
      .execute[HttpResponse]
      .map(response => handleResponse(response, "NI Claim Refund (Update Bank Details)", refundClaimReference, _.json))
  }

  private def extractEtag(response: HttpResponse, caseId: String): String =
    response
      .header("etag")
      .getOrElse(
        throw new InternalServerException(s"etag not found in Pega response for Case ID: $caseId")
      )

  private def buildBankDetailsBody(
    bankAccountName: String,
    sortCode: String,
    accountNumber: String,
    rollNumber: Option[String]
  ): JsValue = {
    val rollNumberJson: JsObject = rollNumber.filter(_.nonEmpty).fold(Json.obj())(r => Json.obj("rollNumber" -> r))
    Json.obj(
      "refundClaimBankDetails" -> (Json.obj(
        "bankAccountName" -> bankAccountName,
        "sortCode"        -> sortCode,
        "accountNumber"   -> accountNumber
      ) ++ rollNumberJson)
    )
  }
}
