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

import java.util.Date

import play.api.Logger
import play.api.libs.json._
import play.api.libs.openid.Errors.BAD_RESPONSE
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.config.DesConnectorConfig
import uk.gov.hmrc.gform.sharedmodel.formtemplate.HmrcTaxPeriod
import uk.gov.hmrc.gform.wshttp.WSHttp

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse, JsValidationException, Upstream5xxResponse }
import uk.gov.hmrc.http.logging.Authorization
import play.api.http.Status

class DesConnector(wSHttp: WSHttp, baseUrl: String, desConfig: DesConnectorConfig) {

  val lookupJson: JsValue =
    Json.parse("""{
       "regime": "REGIMETOGOHERE",
       "requiresNameMatch": false,
       "isAnAgent": false
      }""") //TODO add in actual regime we are looking for

  def lookupAddress(utr: String)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[AddressDes] = {
    implicit val hc = HeaderCarrier(
      extraHeaders = Seq("Environment" -> desConfig.environment),
      authorization = Some(Authorization(s"Bearer ${desConfig.authorizationToken}")))
    Logger.info(s"Des lookup, UTR: '$utr', ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")
    wSHttp.POST[JsValue, AddressDes](s"$baseUrl${desConfig.basePath}/registration/organisation/utr/$utr", lookupJson)
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
    val response = wSHttp.GET[HttpResponse](value)
    response.map { i =>
      i.status match {
        case Status.OK        => i.json.validate[Obligation].get
        case Status.NOT_FOUND => Obligation(List())
        case _                => throw new Upstream5xxResponse("Misc Failure", 500, 500)
      }
    }
  }
}

case class AddressDes(postalCode: String)

object AddressDes {
  implicit val format: OFormat[AddressDes] = Json.format[AddressDes]
}

case class Identification(incomeSourceType: String, referenceNumber: String, referenceType: String)

object Identification {
  implicit val format: OFormat[Identification] = Json.format[Identification]
}

case class ObligationDetail(
  status: String,
  inboundCorrespondenceFromDate: Date,
  inboundCorrespondenceToDate: Date,
  inboundCorrespondenceDueDate: Date,
  periodKey: String)

object ObligationDetail {
  implicit val format: OFormat[ObligationDetail] = Json.format[ObligationDetail]
}

case class TaxPeriodDes(identification: Identification, obligationDetails: List[ObligationDetail])

object TaxPeriodDes {
  implicit val format: OFormat[TaxPeriodDes] = Json.format[TaxPeriodDes]
}

case class ObligationDetails(obligationDetails: List[ObligationDetail])

object ObligationDetails {
  implicit val format: OFormat[ObligationDetails] = Json.format[ObligationDetails]
}

case class Obligation(obligations: List[ObligationDetails])

object Obligation {
  implicit val format: OFormat[Obligation] = Json.format[Obligation]
}

case class TaxResponse(id: HmrcTaxPeriod, obligation: Obligation)

object TaxResponse {
  implicit val format: OFormat[TaxResponse] = Json.format[TaxResponse]
}

case class NoPeriods(code: String, reason: String)

object NoPeriods {
  implicit val format: OFormat[NoPeriods] = Json.format[NoPeriods]
}
