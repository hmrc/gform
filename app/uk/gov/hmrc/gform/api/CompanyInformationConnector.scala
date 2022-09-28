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

import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import org.slf4j.{ Logger, LoggerFactory }
import play.api.libs.json.{ Format, Json, OFormat, Reads, __ }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.OFormatWithTemplateReadFallback
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, ServiceCallResponse, ServiceResponse }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpReads.Implicits.readFromJson

import scala.concurrent.{ ExecutionContext, Future }

trait CompanyInformationConnector[F[_]] {
  def companyProfile(companyNumber: CompanyProfile.Request)(implicit
    hc: HeaderCarrier
  ): F[ServiceCallResponse[CompanyProfile.Response]]
}

class CompanyInformationAsyncConnector(ws: WSHttp, baseUrl: String, apiKey: String)(implicit ex: ExecutionContext)
    extends CompanyInformationConnector[Future] {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  override def companyProfile(
    companyNumber: CompanyProfile.Request
  )(implicit hc: HeaderCarrier): Future[ServiceCallResponse[CompanyProfile.Response]] = {
    val url = s"$baseUrl/company/${companyNumber.companyNumber}"
    val h = Seq("Authorization" -> apiKey)

    ws.GET[CompanyProfile.Response](url, headers = h)
      .map { response =>
        ServiceResponse(response)
      }
      .recover { case ex =>
        logger.error("Unknown problem when calling company profile", ex)
        CannotRetrieveResponse
      }
  }
}

object CompanyProfile {
  case class Request(companyNumber: String)

  def create(companyNumber: String) = Request(companyNumber)

  object Request {
    implicit val format: Format[Request] = Json.format[Request]
  }

  case class RegisteredAddress(
    addressLine1: String,
    addressLine2: Option[String],
    postalCode: String,
    locality: String,
    region: Option[String]
  )

  object RegisteredAddress {
    private val templateReads: Reads[RegisteredAddress] =
      ((__ \ "address_line_1").read[String] and
        (__ \ "address_line_2").readNullable[String] and
        (__ \ "postal_code").read[String] and
        (__ \ "locality").read[String] and
        (__ \ "region").readNullable[String])(RegisteredAddress.apply _)

    implicit val format: OFormat[RegisteredAddress] = OFormatWithTemplateReadFallback(templateReads)
  }

  case class Response(
    name: String,
    status: String,
    registeredAddress: RegisteredAddress
  )

  object Response {

    private val templateReads: Reads[Response] =
      ((__ \ "company_name").read[String] and
        (__ \ "company_status").read[String] and
        (__ \ "registered_office_address").read[RegisteredAddress])(Response.apply _)

    implicit val format: OFormat[Response] = OFormatWithTemplateReadFallback(templateReads)
  }
}
