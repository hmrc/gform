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

package uk.gov.hmrc.gform.sharedmodel.sdes

import julienrf.json.derived
import play.api.libs.json.OFormat

import java.time.Instant

case class SdesSubmissionPageData(
  sdesSubmissions: List[SdesSubmissionData],
  count: Long,
  countAll: Long
)

object SdesSubmissionPageData {
  implicit val format: OFormat[SdesSubmissionPageData] = derived.oformat()
}

case class SdesSubmissionData(
  envelopeId: String,
  formTemplateId: String,
  submissionRef: String,
  submittedAt: Instant,
  status: String,
  failureReason: String
)

object SdesSubmissionData {
  implicit val format: OFormat[SdesSubmissionData] = derived.oformat()
}
