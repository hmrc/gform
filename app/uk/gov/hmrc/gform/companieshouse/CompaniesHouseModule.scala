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

import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.companieshouse.CompaniesHouseModule.CompaniesHouseAPIConfig
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule

import java.net.URI
import java.nio.charset.StandardCharsets
import java.util.Base64
import scala.concurrent.ExecutionContext

class CompaniesHouseModule(
  configModule: ConfigModule,
  auditingModule: AuditingModule,
  wSHttpModule: WSHttpModule
)(implicit ex: ExecutionContext) {

  val companiesHouseConfig: CompaniesHouseAPIConfig = readCompaniesHouseConfig

  val companiesHouseAuditService =
    new CompaniesHouseAuditService(auditingModule.auditConnector, configModule.configuration)
  val companiesHouseConnector =
    new CompaniesHouseConnector(wSHttpModule.httpClient, companiesHouseConfig, companiesHouseAuditService)
  val companiesHouseService =
    new CompaniesHouseService(companiesHouseConnector, configModule.configuration, companiesHouseAuditService)
  val companiesHouseController = new CompaniesHouseController(companiesHouseService, configModule.controllerComponents)

  private def readCompaniesHouseConfig = {
    val basePath = configModule.serviceConfig.getConfString("companies-house-api.base-path", "")
    val rawAuthToken = configModule.serviceConfig.getConfString(
      "companies-house-api.authorization-token",
      throw new IllegalStateException("Missing config companies-house-api.authorization-token")
    )

    CompaniesHouseAPIConfig(
      URI.create(s"${configModule.serviceConfig.baseUrl("companies-house-api")}$basePath"),
      Base64.getEncoder.encodeToString(s"$rawAuthToken:".getBytes(StandardCharsets.UTF_8))
    )
  }
}

object CompaniesHouseModule {
  case class CompaniesHouseAPIConfig(baseUrl: URI, authorizationToken: String)
}
