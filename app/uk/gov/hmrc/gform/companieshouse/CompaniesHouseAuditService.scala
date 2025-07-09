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
import play.api.Configuration
import play.api.libs.json.{ JsValue, Json }
import play.api.mvc.Request
import uk.gov.hmrc.play.audit.AuditExtensions.auditHeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.{ DataEvent, ExtendedDataEvent }
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendHeaderCarrierProvider

import scala.concurrent.{ ExecutionContext, Future }

class CompaniesHouseAuditService(
  auditConnector: AuditConnector,
  configuration: Configuration
)(implicit executionContext: ExecutionContext)
    extends BackendHeaderCarrierProvider {

  def companyInformationRequest(companyNumber: String)(implicit request: Request[?]): Future[Unit] = {
    val detail = Map("companyNumber" -> companyNumber)
    sendEvent(eventFor("companyInformationRequest", detail, "Companies House - Company Information Request"))
  }

  def companyOfficersRequest(companyNumber: String, surname: Option[String] = None)(implicit
    request: Request[?]
  ): Future[Unit] = {
    val detail = Map(
      "companyNumber" -> companyNumber,
      "surname"       -> surname.getOrElse("-")
    )
    sendEvent(eventFor("companyOfficersRequest", detail, "Companies House - Company Officers Request"))
  }

  def successfulCompanyInformationResponse(response: JsValue)(implicit request: Request[?]): Future[Unit] = {
    val companyNumber = (response \ "company_number").asOpt[String].getOrElse("-")
    val companyName = (response \ "company_name").asOpt[String].getOrElse("-")
    val companyStatus = (response \ "company_status").asOpt[String].getOrElse("-")

    val detail = Map(
      "companyNumber" -> companyNumber,
      "companyName"   -> companyName,
      "companyStatus" -> companyStatus
    )
    sendEvent(eventFor("companyInformationResponse", detail, "Companies House - Company Information Response"))
  }

  def failedCompanyInformationResponse(errorCode: Int, errorMessage: String)(implicit
    request: Request[?]
  ): Future[Unit] = {
    val detail = Map(
      "errorCode" -> errorCode.toString,
      "error"     -> errorMessage
    )
    sendEvent(eventFor("companyInformationResponse", detail, "Companies House - Company Information Response"))
  }

  def successfulCompanyOfficersResponse(
    response: OfficersResponse,
    companyNumber: String,
    surnameFilter: Option[String]
  )(implicit request: Request[?]): Future[Unit] = {
    val limit = configuration.get[Int]("companies-house.auditing.officers.response.limit")

    val surname = surnameFilter.getOrElse("-") // this refuses to compile when inlined for some reason

    val detail = Json.obj(
      "surname"       -> surname,
      "companyNumber" -> companyNumber,
      "activeCount"   -> response.activeCount,
      "totalResults"  -> response.totalResults,
      "officers" -> response.items.take(limit).map { officer =>
        Json.obj(
          "name"         -> officer.name,
          "occupation"   -> officer.occupation,
          "officer_role" -> officer.officerRole
        )
      }
    )

    auditConnector
      .sendExtendedEvent(
        extendedEventFor("companyOfficersResponse", detail, "Companies House - Company Officers Response")
      )
      .map(_ => ())
  }

  def failedCompanyOfficersResponse(errorCode: Int, errorMessage: String)(implicit
    request: Request[?]
  ): Future[Unit] = {
    val detail = Map(
      "errorCode" -> errorCode.toString,
      "error"     -> errorMessage
    )
    sendEvent(eventFor("companyOfficersResponse", detail, "Companies House - Company Officers Response"))
  }

  def suspiciousActivity(reason: String)(implicit request: Request[?]): Future[Unit] = {
    val detail = Map(
      "context" -> "Incoming payload; likely tampering",
      "reason"  -> reason
    )
    sendEvent(eventFor("SuspiciousActivity", detail, "Companies House - Suspicious Activity"))
  }

  private def eventFor(
    auditType: String,
    details: Map[String, String],
    transactionName: String,
    tagOverrides: Map[String, String] = Map()
  )(implicit
    request: Request[?]
  ) = {
    val hcTags = hc.toAuditTags(transactionName, request.path) ++ tagOverrides
    val hcDetails = hc.toAuditDetails() ++ details
    DataEvent(auditType = auditType, tags = hcTags, detail = hcDetails, auditSource = "companies-house-api-proxy")
  }

  private def extendedEventFor(
    auditType: String,
    details: JsValue,
    transactionName: String,
    tagOverrides: Map[String, String] = Map()
  )(implicit
    request: Request[?]
  ) = {
    val hcTags = hc.toAuditTags(transactionName, request.path) ++ tagOverrides
    ExtendedDataEvent(auditType = auditType, tags = hcTags, detail = details, auditSource = "companies-house-api-proxy")
  }

  private def sendEvent(dataEvent: DataEvent) = {
    val cleanedDataEvent = dataEvent.copy(detail = dataEvent.detail.map {
      case (key: String, value: String) if value.isEmpty || value == "None" => (key, "-")
      case (key: String, value: String)                                     => key -> value
    })
    auditConnector.sendEvent(cleanedDataEvent).map(_ => ())
  }

}
