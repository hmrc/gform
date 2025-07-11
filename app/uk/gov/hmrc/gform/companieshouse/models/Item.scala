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

import java.time.LocalDate

case class Item(
  address: Option[Address],
  appointedOn: Option[LocalDate],
  countryOfResidence: Option[String],
  dateOfBirth: Option[DateOfBirth],
  formerNames: Option[List[FormerNames]],
  identification: Option[Identification],
  links: ItemLinks,
  name: String,
  nationality: Option[String],
  occupation: Option[String],
  officerRole: String,
  resignedOn: Option[LocalDate]
)

object Item {
  implicit val format: Format[Item] = (
    (__ \ "address").formatNullable[Address] and
      (__ \ "appointed_on").formatNullable[LocalDate] and
      (__ \ "country_of_residence").formatNullable[String] and
      (__ \ "date_of_birth").formatNullable[DateOfBirth] and
      (__ \ "former_names").formatNullable[List[FormerNames]] and
      (__ \ "identification").formatNullable[Identification] and
      (__ \ "links").format[ItemLinks] and
      (__ \ "name").format[String] and
      (__ \ "nationality").formatNullable[String] and
      (__ \ "occupation").formatNullable[String] and
      (__ \ "officer_role").format[String] and
      (__ \ "resigned_on").formatNullable[LocalDate]
  )(Item.apply, unlift(Item.unapply))
}
