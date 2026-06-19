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

package uk.gov.hmrc.gform.companieshouse

import play.api.Logging
import play.api.libs.json.JsValue
import uk.gov.hmrc.gform.companieshouse.CompaniesHouseModule.CompaniesHouseAPIConfig
import uk.gov.hmrc.gform.companieshouse.models.OfficersResponse
import uk.gov.hmrc.gform.sharedmodel.DataRetrieveDefinitions
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{ ForbiddenException, HeaderCarrier, HeaderNames, HttpResponse, InternalServerException, NotFoundException, StringContextOps, UnauthorizedException }
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendHeaderCarrierProvider

import java.net.URI
import scala.concurrent.{ ExecutionContext, Future }

class CompaniesHouseConnector(
  http: HttpClientV2,
  config: CompaniesHouseAPIConfig
)(implicit ec: ExecutionContext)
    extends BackendHeaderCarrierProvider with Logging {

  private val serviceUrl: URI = config.baseUrl

  private val headersWithApiKey = HeaderNames.authorisation -> s"Basic ${config.authorizationToken}"
  private val companyNumberParam = "companyNumber"

  def getCompany(companyNumber: String): Future[JsValue] = {
    val urlPath = DataRetrieveDefinitions.companyHouseProfile.getFilledPath(
      Map(companyNumberParam -> companyNumber)
    )
    val url = s"$serviceUrl$urlPath"
    http
      .get(url"$url")(HeaderCarrier())
      .setHeader(headersWithApiKey)
      .withProxy
      .execute[HttpResponse]
      .map {
        handleJsonResponse(
          _,
          "Company profile"
        )
      }
  }

  def getCompanyOfficers(companyNumber: String): Future[OfficersResponse] = {
    val urlPath = DataRetrieveDefinitions.companyHouseActiveOfficers.getFilledPath(
      Map(companyNumberParam -> companyNumber)
    )
    val url = s"$serviceUrl$urlPath"
    http
      .get(url"$url")(HeaderCarrier())
      .setHeader(headersWithApiKey)
      .withProxy
      .execute[HttpResponse]
      .map(handleOfficersResponse)
  }

  def getCompanyOfficersPaged(companyNumber: String)(
    pageIndex: Int
  ): Future[OfficersResponse] = {
    val urlPath = DataRetrieveDefinitions.companyHouseActiveOfficers.getFilledPath(
      Map(companyNumberParam -> companyNumber)
    )
    val url = s"$serviceUrl$urlPath?items_per_page=100&start_index=$pageIndex"
    http
      .get(url"$url")(HeaderCarrier())
      .setHeader(headersWithApiKey)
      .withProxy
      .execute[HttpResponse]
      .map(handleOfficersResponse)
  }

  def getCompanyInsolvency(companyNumber: String): Future[JsValue] = {
    val urlPath = DataRetrieveDefinitions.companyHouseInsolvency.getFilledPath(
      Map(companyNumberParam -> companyNumber)
    )
    val url = s"$serviceUrl$urlPath"
    http
      .get(url"$url")(HeaderCarrier())
      .setHeader(headersWithApiKey)
      .withProxy
      .execute[HttpResponse]
      .map(
        handleJsonResponse(
          _,
          "Company insolvency information"
        )
      )
  }

  private def handleJsonResponse(
    response: HttpResponse,
    resource: String
  ): JsValue =
    response.status match {
      case 200 =>
        response.json
      case 404 =>
        throw new NotFoundException(s"$resource not found")
      case 401 =>
        logger.error(
          s"Received unauthorized response. Check whether the api-key permissions are changed. ${response.body}"
        )
        throw new UnauthorizedException("Unauthorized request to companies house")
      case 403 =>
        logger.error(s"Received forbidden response. Check whether the api-key has expired. ${response.body}")
        throw new ForbiddenException("Forbidden request to companies house")
      case 429 =>
        val message = "Received rate limit response from companies house"
        logger.error(message)
        throw new InternalServerException(message)
      case status =>
        logger.error(s"Received unexpected status $status. ${response.body}")
        throw new InternalServerException("Unexpected response code from companies house")
    }

  private def handleOfficersResponse(response: HttpResponse): OfficersResponse =
    response.status match {
      case 200 =>
        response.json.validate[OfficersResponse].recoverTotal { errors =>
          logger.error(s"Unable to parse officers response: $errors")
          throw new IllegalArgumentException("Unparseable officers response")
        }
      case 404 =>
        throw new NotFoundException("Company profile not found")
      case 401 =>
        logger.error(
          s"Received unauthorized response. Check whether the api-key permissions are changed. ${response.body}"
        )
        throw new UnauthorizedException("Unauthorized request to companies house")
      case 403 =>
        logger.error(s"Received forbidden response. Check whether the api-key has expired. ${response.body}")
        throw new ForbiddenException("Forbidden request to companies house")
      case 429 =>
        val message = "Received rate limit response from companies house"
        logger.error(message)
        throw new InternalServerException(message)
      case status =>
        logger.error(s"Received unexpected status $status. ${response.body}")
        throw new InternalServerException("Unexpected response code from companies house")
    }

}
