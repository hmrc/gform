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

package uk.gov.hmrc.gform.des

import org.apache.pekko.http.scaladsl.model.StatusCodes
import cats.instances.int._
import cats.instances.string._
import cats.syntax.eq._
import org.slf4j.LoggerFactory
import play.api.libs.json._
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.config.DesConnectorConfig
import uk.gov.hmrc.gform.sharedmodel.des.{ DesAgentDetailsResponse, DesRegistrationRequest, DesRegistrationResponse, DesRegistrationResponseError }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.http._
import uk.gov.hmrc.http.client.HttpClientV2

import java.net.URI
import scala.concurrent.{ ExecutionContext, Future }
import scala.reflect.runtime.universe.TypeTag
import scala.util.{ Failure, Success, Try }

trait DesAlgebra[F[_]] {

  def lookupRegistration(
    utr: String,
    desRegistrationRequest: DesRegistrationRequest
  ): F[ServiceCallResponse[DesRegistrationResponse]]

  def lookupTaxPeriod(idType: String, idNumber: String, regimeType: String): F[ServiceCallResponse[Obligation]]

  def lookupAgentDetails(
    idType: String,
    idNumber: String
  ): F[ServiceCallResponse[DesAgentDetailsResponse]]

  def testOnlyGet(url: String): Future[HttpResponse]
}

class DesConnector(httpClient: HttpClientV2, baseUrl: String, desConfig: DesConnectorConfig)(implicit
  ec: ExecutionContext
) extends DesAlgebra[Future] with LowPriorityHttpReadsJson with HttpReadsEither with HttpReadsHttpResponse {

  private val logger = LoggerFactory.getLogger(getClass)

  private implicit val hc: HeaderCarrier = HeaderCarrier(
    extraHeaders = Seq("Environment" -> desConfig.environment)
  )

  private val authHeaders: (String, String) =
    "Authorization" -> s"Bearer ${desConfig.authorizationToken}"

  def lookupRegistration(
    utr: String,
    desRegistrationRequest: DesRegistrationRequest
  ): Future[ServiceCallResponse[DesRegistrationResponse]] = {

    logger.info(s"Des registration, UTR: '$utr', ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")

    val url = s"$baseUrl${desConfig.basePath}/registration/organisation/utr/$utr"
    httpClient
      .post(url"$url")
      .withBody(Json.toJson(desRegistrationRequest))
      .setHeader(authHeaders)
      .execute[HttpResponse]
      .map { httpResponse =>
        val status = httpResponse.status
        if (status === 404 || status === 400) {
          processResponse[DesRegistrationResponseError, Nothing](httpResponse) { desError =>
            if (desError.code === "NOT_FOUND" || desError.code === "INVALID_UTR") NotFound
            else {
              val jsonResponse = Json.prettyPrint(Json.toJson(desError))
              logger.error(
                s"Problem when calling des registration. Response status: $status, body response: $jsonResponse"
              )
              CannotRetrieveResponse
            }
          }
        } else processResponse[DesRegistrationResponse, DesRegistrationResponse](httpResponse)(ServiceResponse.apply)
      }
      .recover { case ex =>
        logger.error("Unknown problem when calling des registration", ex)
        CannotRetrieveResponse
      }
  }

  private def processResponse[A: Format: TypeTag, B](httpResponse: HttpResponse)(
    f: A => ServiceCallResponse[B]
  ): ServiceCallResponse[B] = {
    val status = httpResponse.status
    Try(httpResponse.json) match {
      case Success(jsValue) =>
        jsValue.validate[A].map(f).recoverTotal { jsError =>
          val tpe = implicitly[TypeTag[A]].tpe
          logger.error(
            s"Unknown problem when calling des registration. Response status was: $status. Expected json for $tpe, but got: $jsError"
          )
          CannotRetrieveResponse
        }
      case Failure(failure) =>
        val tpe = implicitly[TypeTag[A]].tpe
        logger.error(
          s"Unknown problem when calling des registration. Response status was: $status. Expected json for $tpe, but got :",
          failure
        )
        CannotRetrieveResponse
    }
  }

  def lookupTaxPeriod(idType: String, idNumber: String, regimeType: String): Future[ServiceCallResponse[Obligation]] = {
    logger.info(
      s"Des lookup, Tax Periods: '$idType, $idNumber, $regimeType', ${loggingHelpers.cleanHeaderCarrierHeader(hc)}"
    )

    val url = s"$baseUrl${desConfig.basePath}/enterprise/obligation-data/$idType/$idNumber/$regimeType?status=O"

    httpClient
      .get(URI.create(url).toURL)
      .setHeader(authHeaders)
      .execute[Obligation]
      .map(ServiceResponse.apply)
      .recover {
        case UpstreamErrorResponse.WithStatusCode(statusCode) if statusCode == StatusCodes.NotFound.intValue =>
          NotFound
        case other =>
          logger.error("Unknown problem when calling des obligation-data", other)
          CannotRetrieveResponse
      }
  }

  def testOnlyGet(testUrl: String): Future[HttpResponse] = {
    val url = s"$baseUrl${desConfig.basePath}/$testUrl"
    httpClient
      .get(url"$url")
      .setHeader(authHeaders)
      .execute[HttpResponse]
  }

  override def lookupAgentDetails(
    idType: String,
    idNumber: String
  ): Future[ServiceCallResponse[DesAgentDetailsResponse]] = {
    logger.info(
      s"Des agent-details called, ${loggingHelpers.cleanHeaderCarrierHeader(hc)}"
    )

    val url = s"$baseUrl${desConfig.basePath}/registration/personal-details/$idType/$idNumber"
    httpClient
      .get(url"$url")
      .setHeader(authHeaders)
      .execute[HttpResponse]
      .map { httpResponse =>
        val status = httpResponse.status
        if (status === 404 || status === 400) {
          processResponse[DesRegistrationResponseError, Nothing](httpResponse) { desError =>
            if (desError.code === "NOT_FOUND" || desError.code === s"INVALID_${idType.toUpperCase}") NotFound
            else {
              val jsonResponse = Json.prettyPrint(Json.toJson(desError))
              logger.error(
                s"Problem when calling des registration for agent details. Response status: $status, body response: $jsonResponse"
              )
              CannotRetrieveResponse
            }
          }
        } else
          processResponse[DesAgentDetailsResponse, DesAgentDetailsResponse](httpResponse)(ServiceResponse.apply)
      }
      .recover { case ex =>
        logger.error("Unknown problem when calling des registration for agent details", ex)
        CannotRetrieveResponse
      }
  }
}

case class AddressDes(postalCode: String)

object AddressDes {
  implicit val format: OFormat[AddressDes] = Json.format[AddressDes]
}
