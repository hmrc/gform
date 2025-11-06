/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.hip.models

import julienrf.json.derived
import play.api.libs.json.OFormat

case class Employment(
  payeSchemeOperatorName: String,
  payeNumber: String,
  taxDistrictNumber: String,
  employmentSequenceNumber: Int,
  worksNumber: String,
  directorIdentifier: Boolean
)

object Employment {
  implicit val format: OFormat[Employment] = derived.oformat()
}

case class NIEmployments(
  taxYear: Int,
  identifier: String,
  individualsEmploymentDetails: List[Employment],
  moreDataCallbackURL: String
)

object NIEmployments {
  implicit val format: OFormat[NIEmployments] = derived.oformat()
}
