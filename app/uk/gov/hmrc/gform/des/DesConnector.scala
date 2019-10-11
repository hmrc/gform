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

import cats.syntax.eq._
import cats.instances.string._
import cats.instances.int._
import play.api.Logger
import play.api.http.Status
import play.api.libs.json._
import scala.concurrent.{ ExecutionContext, Future }
import scala.reflect.runtime.universe.TypeTag
import scala.util.{ Failure, Success, Try }
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.config.DesConnectorConfig
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, NotFound, ServiceCallResponse, ServiceResponse }
import uk.gov.hmrc.gform.sharedmodel.des.{ DesRegistrationRequest, DesRegistrationResponse, DesRegistrationResponseError }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.gform.sharedmodel.Obligation
import uk.gov.hmrc.http._
import uk.gov.hmrc.http.logging.Authorization

class DesConnector(wSHttp: WSHttp, baseUrl: String, desConfig: DesConnectorConfig)(implicit ec: ExecutionContext) {

  def lookupRegistration(
    utr: String,
    desRegistrationRequest: DesRegistrationRequest): Future[ServiceCallResponse[DesRegistrationResponse]] = {

    implicit val hc = HeaderCarrier(
      extraHeaders = Seq("Environment" -> desConfig.environment),
      authorization = Some(Authorization(s"Bearer ${desConfig.authorizationToken}")))
    Logger.info(s"Des registration, UTR: '$utr', ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")

    wSHttp
      .doPost[DesRegistrationRequest](
        s"$baseUrl${desConfig.basePath}/registration/organisation/utr/$utr",
        desRegistrationRequest,
        List.empty[(String, String)]
      )
      .map { httpResponse =>
        if (httpResponse.status === 400) {
          processResponse[DesRegistrationResponseError, Nothing](httpResponse) { desError =>
            if (desError.code === "NOT_FOUND" || desError.code === "INVALID_UTR") NotFound
            else {
              Logger.error("Problem when calling des registration: " + Json.prettyPrint(Json.toJson(desError)))
              CannotRetrieveResponse
            }
          }
        } else processResponse[DesRegistrationResponse, DesRegistrationResponse](httpResponse)(ServiceResponse.apply)
      }
      .recover {
        case ex =>
          Logger.error("Unknown problem when calling des registration", ex)
          CannotRetrieveResponse
      }
  }

  private def processResponse[A: Format: TypeTag, B](httpResponse: HttpResponse)(
    f: A => ServiceCallResponse[B]
  ): ServiceCallResponse[B] =
    Try(httpResponse.json) match {
      case Success(jsValue) =>
        jsValue.validate[A].map(f).recoverTotal { jsError =>
          val tpe = implicitly[TypeTag[A]].tpe
          Logger.error(s"Unknown problem when calling des registration. Expected json for $tpe, but got: $jsError")
          CannotRetrieveResponse
        }
      case Failure(failure) =>
        val tpe = implicitly[TypeTag[A]].tpe
        val status = httpResponse.status
        Logger.error(
          s"Unknown problem when calling des registration. Response status was: $status. Expected json for $tpe, but got :",
          failure)
        CannotRetrieveResponse
    }

  def lookupTaxPeriod(idType: String, idNumber: String, regimeType: String)(
    implicit hc: HeaderCarrier): Future[Obligation] = {
    implicit val hc = HeaderCarrier(
      extraHeaders = Seq("Environment" -> desConfig.environment),
      authorization = Some(Authorization(s"Bearer ${desConfig.authorizationToken}")))
    Logger.info(
      s"Des lookup, Tax Periods: '$idType, $idNumber, $regimeType', ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")
    val value = s"$baseUrl${desConfig.basePath}/enterprise/obligation-data/$idType/$idNumber/$regimeType?status=O"
    wSHttp.GET[Obligation](value).recover { case _ => Obligation(List()) }
  }
}

case class AddressDes(postalCode: String)

object AddressDes {
  implicit val format: OFormat[AddressDes] = Json.format[AddressDes]
}
