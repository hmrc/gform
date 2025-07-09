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

import play.api.libs.json.Json
import play.api.mvc.{ Action, AnyContent, ControllerComponents }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.http.HttpException

import scala.concurrent.ExecutionContext

class CompaniesHouseController(
  companiesHouseService: CompaniesHouseService,
  controllerComponents: ControllerComponents
)(implicit ec: ExecutionContext)
    extends BaseController(controllerComponents) {

  def findCompany(companyNumber: String): Action[AnyContent] = Action.async { implicit request =>
    companiesHouseService.findCompany(companyNumber).map(Ok(_)).recover {
      case e: HttpException => Status(e.responseCode)
      case _                => ServiceUnavailable
    }
  }

  def findCompanyOfficers(companyNumber: String, surname: Option[String]): Action[AnyContent] = Action.async {
    implicit request =>
      surname match {
        case Some(name) =>
          companiesHouseService
            .findCompanyOfficersBySurname(companyNumber, name)
            .map { response =>
              if (response.items.isEmpty) {
                NotFound
              } else {
                Ok(Json.toJson(response))
              }
            }
            .recover {
              case e: HttpException if e.message == "Received rate limit response from companies house" =>
                InternalServerError(Json.obj("error" -> e.message))
              case e: HttpException => Status(e.responseCode)
              case _                => ServiceUnavailable
            }
        case None =>
          companiesHouseService.findCompanyOfficers(companyNumber).map(response => Ok(Json.toJson(response))).recover {
            case e: HttpException if e.message == "Received rate limit response from companies house" =>
              InternalServerError(Json.obj("error" -> e.message))
            case e: HttpException => Status(e.responseCode)
            case _                => ServiceUnavailable
          }
      }
  }

}
