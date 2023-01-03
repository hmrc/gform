/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform.addresslookup

import cats.data.NonEmptyList
import play.api.libs.json.{ Format, Json }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils

object PostcodeLookup {

  final case class Request(
    postcode: String,
    filter: Option[String]
  )

  object Request {
    implicit val format: Format[Request] = Json.format[Request]
  }

  final case class Response(
    filterDisabled: Boolean,
    addresses: Option[NonEmptyList[PostcodeLookup.AddressRecord]]
  )

  object Response {
    import JsonUtils._
    implicit val format: Format[Response] = Json.format[Response]
  }

  // AddressRecord model taken from https://github.com/hmrc/address-lookup-frontend/tree/f1f4b9b35c51889e36cb5240cfc227f9c85485af/app/address/v2
  final case class AddressRecord(
    id: String,
    uprn: Option[Long],
    address: Address,
    language: String,
    localCustodian: Option[LocalCustodian],
    location: Option[Seq[BigDecimal]],
    blpuState: Option[String],
    logicalState: Option[String],
    streetClassification: Option[String],
    administrativeArea: Option[String] = None,
    poBox: Option[String] = None
  )

  object AddressRecord {
    implicit val format: Format[AddressRecord] = Json.format[AddressRecord]
  }

  final case class Address(
    lines: List[String],
    town: String,
    postcode: String,
    subdivision: Option[Country],
    country: Country
  )

  object Address {
    implicit val format: Format[Address] = Json.format[Address]
  }

  final case class Country(code: String, name: String)

  object Country {
    implicit val format: Format[Country] = Json.format[Country]
  }

  final case class LocalCustodian(code: Int, name: String)

  object LocalCustodian {
    implicit val format: Format[LocalCustodian] = Json.format[LocalCustodian]
  }
}
