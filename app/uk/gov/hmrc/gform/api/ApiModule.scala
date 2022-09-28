/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.api

import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext

class ApiModule(
  configModule: ConfigModule,
  wSHttpModule: WSHttpModule
)(implicit ec: ExecutionContext) {

  private val companyInformationBaseUrl = s"${configModule.serviceConfig.baseUrl("company-information")}"
  private val apiKey = s"${configModule.serviceConfig.getConfString("company-information.apiKey", "")}"

  private implicit val hc = HeaderCarrier()

  val companyInformationConnector =
    new CompanyInformationAsyncConnector(wSHttpModule.auditableWSHttp, companyInformationBaseUrl, apiKey)

  val companyInformationController: CompanyInformationController =
    new CompanyInformationController(configModule.controllerComponents, companyInformationConnector)
}
