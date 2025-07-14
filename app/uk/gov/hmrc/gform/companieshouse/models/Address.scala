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

package uk.gov.hmrc.gform.companieshouse.models

import play.api.libs.functional.syntax.{ toFunctionalBuilderOps, unlift }
import play.api.libs.json.{ Format, __ }

case class Address(
  addressLine1: Option[String],
  addressLine2: Option[String],
  careOf: Option[String],
  country: Option[String],
  locality: Option[String],
  poBox: Option[String],
  postalCode: Option[String],
  premises: Option[String],
  region: Option[String]
)

object Address {
  implicit val format: Format[Address] = (
    (__ \ "address_line_1").formatNullable[String] and
      (__ \ "address_line_2").formatNullable[String] and
      (__ \ "care_of").formatNullable[String] and
      (__ \ "country").formatNullable[String] and
      (__ \ "locality").formatNullable[String] and
      (__ \ "po_box").formatNullable[String] and
      (__ \ "postal_code").formatNullable[String] and
      (__ \ "premises").formatNullable[String] and
      (__ \ "region").formatNullable[String]
  )(Address.apply, unlift(Address.unapply))
}
