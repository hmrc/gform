/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.testonly

import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.domain.Nino

case class EnrolmentData(name: String, state: String, taxIdentifier: List[TaxIdentifierData])

object EnrolmentData {
  implicit val format: OFormat[EnrolmentData] = derived.oformat()
}

case class TaxIdentifierData(key: String, value: String)
object TaxIdentifierData {
  implicit val format: OFormat[TaxIdentifierData] = derived.oformat()
}

case class ItmpAddress(
  line1: Option[String],
  line2: Option[String],
  line3: Option[String],
  line4: Option[String],
  line5: Option[String],
  postCode: Option[String],
  countryCode: Option[String],
  countryName: Option[String]
)

object ItmpAddress {
  implicit val format: OFormat[ItmpAddress] = derived.oformat()
}

case class ItmpData(
  givenName: Option[String],
  middleName: Option[String],
  familyName: Option[String],
  birthdate: Option[String],
  address: ItmpAddress
)

object ItmpData {
  implicit val writes: OFormat[ItmpData] = derived.oformat()
}

case class AgentData(
  agentId: Option[String] = None,
  agentCode: Option[String] = None,
  agentFriendlyName: Option[String] = None
)

object AgentData {
  implicit val writes: OFormat[AgentData] = derived.oformat()
}

case class GovernmentGatewayFormData(
  redirectionUrl: String,
  credentialStrength: String,
  confidenceLevel: String,
  credId: String,
  nino: Option[Nino],
  enrolments: List[EnrolmentData],
  affinityGroup: String,
  credentialRole: String,
  usersName: Option[String],
  email: Option[String],
  gatewayToken: Option[String],
  groupIdentifier: Option[String],
  itmpData: Option[ItmpData],
  agent: Option[AgentData]
)

object GovernmentGatewayFormData {
  implicit val format: OFormat[GovernmentGatewayFormData] = derived.oformat()
}
