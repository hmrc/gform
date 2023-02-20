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

package uk.gov.hmrc.gform.sharedmodel.des

import play.api.libs.json.Reads
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class EmploymentsResponse(
  employerName: String,
  sequenceNumber: Int,
  worksNumber: String,
  taxDistrictNumber: String,
  payeNumber: String,
  director: Boolean
)

object EmploymentsResponse {

  implicit val apiReads: Reads[EmploymentsResponse] =
    ((__ \ "employerName").read[String] and
      (__ \ "sequenceNumber").read[Int] and
      (__ \ "worksNumber").read[String] and
      (__ \ "taxDistrictNumber").read[String] and
      (__ \ "payeNumber").read[String] and
      (__ \ "director").read[Boolean])(EmploymentsResponse.apply _)

  implicit val writer: Writes[EmploymentsResponse] = Json.format
}
