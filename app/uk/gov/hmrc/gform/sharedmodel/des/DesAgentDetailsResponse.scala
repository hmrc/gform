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

import play.api.libs.json.{ Json, OFormat }

case class DesAgentDetailsResponse(
  contactDetails: AgentContactDetails,
  agencyDetails: AgencyDetails
)

object DesAgentDetailsResponse {
  implicit val format: OFormat[DesAgentDetailsResponse] = Json.format[DesAgentDetailsResponse]
}

case class AgentContactDetails(phoneNumber: Option[String])

object AgentContactDetails {
  implicit val format: OFormat[AgentContactDetails] = Json.format[AgentContactDetails]
}

case class AgencyDetails(agencyName: Option[String], agencyAddress: Option[Address], agencyEmail: Option[String])

object AgencyDetails {
  implicit val format: OFormat[AgencyDetails] = Json.format[AgencyDetails]
}
