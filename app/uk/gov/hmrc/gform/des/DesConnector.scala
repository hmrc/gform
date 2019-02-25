/*
 * Copyright 2019 HM Revenue & Customs
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

import play.api.Logger
import play.api.http.Status
import play.api.libs.json._
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.config.DesConnectorConfig
import uk.gov.hmrc.gform.sharedmodel.des.{ DesRegistrationRequest, DesRegistrationResponse }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.gform.sharedmodel.Obligation
import uk.gov.hmrc.http._
import uk.gov.hmrc.http.logging.Authorization

class DesConnector(wSHttp: WSHttp, baseUrl: String, desConfig: DesConnectorConfig) {

  def lookupRegistration(utr: String, desRegistrationRequest: DesRegistrationRequest)(
    implicit ex: ExecutionContext): Future[DesRegistrationResponse] = {
    implicit val hc = HeaderCarrier(
      extraHeaders = Seq("Environment" -> desConfig.environment),
      authorization = Some(Authorization(s"Bearer ${desConfig.authorizationToken}")))
    Logger.info(s"Des registration, UTR: '$utr', ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")
    wSHttp.POST[DesRegistrationRequest, DesRegistrationResponse](
      s"$baseUrl${desConfig.basePath}/registration/organisation/utr/$utr",
      desRegistrationRequest)

  }

  def lookupTaxPeriod(idType: String, idNumber: String, regimeType: String)(
    implicit hc: HeaderCarrier,
    ex: ExecutionContext): Future[Obligation] = {
    implicit val hc = HeaderCarrier(
      extraHeaders = Seq("Environment" -> desConfig.environment),
      authorization = Some(Authorization(s"Bearer ${desConfig.authorizationToken}")))
    Logger.info(
      s"Des lookup, Tax Periods: '$idType, $idNumber, $regimeType', ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")
    val value = s"$baseUrl${desConfig.basePath}/enterprise/obligation-data/$idType/$idNumber/$regimeType?status=O"
    wSHttp.GET[Obligation](value).recover { case _: NotFoundException => Obligation(List()) }
  }
}

case class AddressDes(postalCode: String)

object AddressDes {
  implicit val format: OFormat[AddressDes] = Json.format[AddressDes]
}
