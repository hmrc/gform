package uk.gov.hmrc.gform.companieshouse

import play.api.Logging
import play.api.libs.json.JsValue
import play.api.mvc.Request
import uk.gov.hmrc.gform.auditing.AuditService
import uk.gov.hmrc.gform.config.{AppConfig, CompaniesHouseConnectorConfig}
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.{ForbiddenException, HeaderCarrier, HeaderNames, HttpResponse, InternalServerException, NotFoundException, StringContextOps, UnauthorizedException}
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendHeaderCarrierProvider

import scala.concurrent.{ExecutionContext, Future}

class CompaniesHouseConnector(
                               http: WSHttp,
                               config: CompaniesHouseConnectorConfig
                             )(implicit ec: ExecutionContext) extends BackendHeaderCarrierProvider
  with Logging {

  private val headersWithApiKey = HeaderNames.authorisation -> s"Basic ${config.authorizationToken}"

  def getCompany(companyNumber: String)(implicit request: Request[?]): Future[JsValue] = {
    //auditService.companyInformationRequest(companyNumber)
//    http
//      .get(url"$serviceUrl/company/$companyNumber")(HeaderCarrier())
//      .setHeader(headersWithApiKey)
//      .withProxy
//      .execute[HttpResponse]
//      .map { response =>

    http.doGet(s"${config.basePath}/company/$companyNumber", Seq(headersWithApiKey))
      .map { response =>
        response.status match {
          case 200 =>
//            auditService.successfulCompanyInformationResponse(response.json)
            response.json
          case 404 =>
//            auditService.failedCompanyOfficersResponse(404, "Company profile not found")
            throw new NotFoundException("Company profile not found")
          case 401 =>
            logger.error(s"Received unauthorized response. Check whether the api-key permissions are changed. ${response.body}")
//            auditService.failedCompanyOfficersResponse(401, "Unauthorized request to companies house")
            throw new UnauthorizedException("Unauthorized request to companies house")
          case 403 =>
            logger.error(s"Received forbidden response. Check whether the api-key has expired. ${response.body}")
//            auditService.failedCompanyOfficersResponse(403, "Forbidden request to companies house")
            throw new ForbiddenException("Forbidden request to companies house")
          case 429 =>
            val message = "Received rate limit response from companies house"
            logger.error(message)
            throw new InternalServerException(message)
          case status =>
            logger.error(s"Received unexpected status $status. ${response.body}")
//            auditService.failedCompanyOfficersResponse(status, "Unexpected response code from companies house")
            throw new InternalServerException("Unexpected response code from companies house")
        }
      }
  }

}
