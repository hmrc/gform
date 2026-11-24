/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.integrationFramework

import julienrf.json.derived
import play.api.libs.json.OFormat

case class RegistrationValidityPeriod(
  status: String,
  validFrom: String,
  validTo: Option[String]
)

object RegistrationValidityPeriod {
  implicit val format: OFormat[RegistrationValidityPeriod] = derived.oformat()
}

case class VerifyPodStatusResponse(
  matchFound: Boolean,
  matchNotFoundReason: Option[String],
  exporterCompanyName: Option[String],
  podReference: Option[String],
  currentRegistrationStatus: Option[String],
  registrationValidityPeriods: Option[List[RegistrationValidityPeriod]]
) {
  def transform: List[FlattenedVerifyPodStatusResponse] =
    registrationValidityPeriods match {
      case Some(periods) if periods.nonEmpty =>
        periods.map { period =>
          FlattenedVerifyPodStatusResponse(
            matchFound,
            matchNotFoundReason,
            exporterCompanyName,
            podReference,
            currentRegistrationStatus,
            Some(period.status),
            Some(period.validFrom),
            period.validTo
          )
        }
      case _ =>
        List(
          FlattenedVerifyPodStatusResponse(
            matchFound,
            matchNotFoundReason,
            exporterCompanyName,
            podReference,
            currentRegistrationStatus,
            None,
            None,
            None
          )
        )
    }
}

object VerifyPodStatusResponse {
  implicit val format: OFormat[VerifyPodStatusResponse] = derived.oformat()
}

case class FlattenedVerifyPodStatusResponse(
  matchFound: Boolean,
  matchNotFoundReason: Option[String],
  exporterCompanyName: Option[String],
  podReference: Option[String],
  currentRegistrationStatus: Option[String],
  periodStatus: Option[String],
  periodValidFrom: Option[String],
  periodValidTo: Option[String]
)

object FlattenedVerifyPodStatusResponse {
  implicit val format: OFormat[FlattenedVerifyPodStatusResponse] = derived.oformat()
}
