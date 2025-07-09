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

import models.OfficersResponse
import play.api.Logging
import play.api.libs.json.JsValue
import play.api.mvc.Request
import uk.gov.hmrc.gform.companieshouse.CompaniesHouseModule.CompaniesHouseAPIConfig
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{ ForbiddenException, HeaderCarrier, HeaderNames, HttpResponse, InternalServerException, NotFoundException, StringContextOps, UnauthorizedException }
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendHeaderCarrierProvider

import java.net.URI
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw

class CompaniesHouseConnector(
  http: HttpClientV2,
  config: CompaniesHouseAPIConfig,
  auditService: CompaniesHouseAuditService
)(implicit ec: ExecutionContext)
    extends BackendHeaderCarrierProvider with Logging {

  private val serviceUrl: URI = config.baseUrl

  private val headersWithApiKey = HeaderNames.authorisation -> s"Basic ${config.authorizationToken}"

  def getCompany(companyNumber: String)(implicit request: Request[?]): Future[JsValue] = {
    auditService.companyInformationRequest(companyNumber)
    http
      .get(url"$serviceUrl/company/$companyNumber")(HeaderCarrier())
      .setHeader(headersWithApiKey)
      .withProxy
      .execute[HttpResponse]
      .map { response =>
        response.status match {
          case 200 =>
            auditService.successfulCompanyInformationResponse(response.json)
            response.json
          case 404 =>
            auditService.failedCompanyOfficersResponse(404, "Company profile not found")
            throw new NotFoundException("Company profile not found")
          case 401 =>
            logger.error(
              s"Received unauthorized response. Check whether the api-key permissions are changed. ${response.body}"
            )
            auditService.failedCompanyOfficersResponse(401, "Unauthorized request to companies house")
            throw new UnauthorizedException("Unauthorized request to companies house")
          case 403 =>
            logger.error(s"Received forbidden response. Check whether the api-key has expired. ${response.body}")
            auditService.failedCompanyOfficersResponse(403, "Forbidden request to companies house")
            throw new ForbiddenException("Forbidden request to companies house")
          case 429 =>
            val message = "Received rate limit response from companies house"
            logger.error(message)
            throw new InternalServerException(message)
          case status =>
            logger.error(s"Received unexpected status $status. ${response.body}")
            auditService.failedCompanyOfficersResponse(status, "Unexpected response code from companies house")
            throw new InternalServerException("Unexpected response code from companies house")
        }
      }
  }

  def getCompanyOfficers(companyNumber: String)(implicit request: Request[?]): Future[OfficersResponse] = {
    auditService.companyOfficersRequest(companyNumber)
    http
      .get(url"$serviceUrl/company/$companyNumber/officers")(HeaderCarrier())
      .setHeader(headersWithApiKey)
      .withProxy
      .execute[HttpResponse]
      .map(handleOfficersResponse)
  }

  def getCompanyOfficersPaged(companyNumber: String, surname: String)(
    pageIndex: Int
  )(implicit request: Request[?]): Future[OfficersResponse] = {
    auditService.companyOfficersRequest(companyNumber, Some(surname))
    http
      .get(url"$serviceUrl/company/$companyNumber/officers?items_per_page=100&start_index=$pageIndex")(HeaderCarrier())
      .setHeader(headersWithApiKey)
      .withProxy
      .execute[HttpResponse]
      .map(handleOfficersResponse)
  }

  private def handleOfficersResponse(response: HttpResponse)(implicit request: Request[?]): OfficersResponse =
    response.status match {
      case 200 =>
        response.json.validate[OfficersResponse].recoverTotal { errors =>
          logger.error(s"Unable to parse officers response: $errors")
          throw new IllegalArgumentException("Unparseable officers response")
        }
      case 404 =>
        auditService.failedCompanyOfficersResponse(404, "Company profile not found")
        throw new NotFoundException("Company profile not found")
      case 401 =>
        logger.error(
          s"Received unauthorized response. Check whether the api-key permissions are changed. ${response.body}"
        )
        auditService.failedCompanyOfficersResponse(401, "Unauthorized request to companies house")
        throw new UnauthorizedException("Unauthorized request to companies house")
      case 403 =>
        logger.error(s"Received forbidden response. Check whether the api-key has expired. ${response.body}")
        auditService.failedCompanyOfficersResponse(403, "Forbidden request to companies house")
        throw new ForbiddenException("Forbidden request to companies house")
      case 429 =>
        val message = "Received rate limit response from companies house"
        logger.error(message)
        throw new InternalServerException(message)
      case status =>
        logger.error(s"Received unexpected status $status. ${response.body}")
        auditService.failedCompanyOfficersResponse(status, "Unexpected response code from companies house")
        throw new InternalServerException("Unexpected response code from companies house")
    }

}
