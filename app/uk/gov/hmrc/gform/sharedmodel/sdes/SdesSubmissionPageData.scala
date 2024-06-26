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

import play.api.libs.json.{ Format, Json, OFormat }
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination.Dms

import java.time.Instant

case class SdesSubmissionPageData(
  sdesSubmissions: List[SdesSubmissionData],
  count: Long
)

object SdesSubmissionPageData {
  implicit val format: OFormat[SdesSubmissionPageData] = Json.format
}

case class SdesSubmissionData(
  correlationId: CorrelationId,
  envelopeId: EnvelopeId,
  formTemplateId: FormTemplateId,
  submissionRef: SubmissionRef,
  submittedAt: Option[Instant],
  status: NotificationStatus,
  failureReason: String,
  lastUpdated: Option[Instant],
  numberOfFiles: Int,
  uploadCount: Int,
  size: Long,
  destination: SdesDestination
)

object SdesSubmissionData {

  def fromSdesSubmission(sdesSubmission: SdesSubmission, numberOfFiles: Int = 0, uploadCount: Int = 0, size: Long = 0) =
    SdesSubmissionData(
      sdesSubmission._id,
      sdesSubmission.envelopeId,
      sdesSubmission.formTemplateId,
      sdesSubmission.submissionRef,
      sdesSubmission.submittedAt,
      sdesSubmission.status,
      sdesSubmission.failureReason.getOrElse(""),
      sdesSubmission.lastUpdated,
      numberOfFiles,
      uploadCount,
      size,
      sdesSubmission.destination.getOrElse(Dms)
    )

  implicit val correlationIdFormat: Format[CorrelationId] = CorrelationId.vformat
  implicit val envelopeIdFormat: Format[EnvelopeId] = EnvelopeId.vformat
  implicit val formTemplateIdFormat: Format[FormTemplateId] = FormTemplateId.vformat
  implicit val submissionRefFormat: Format[SubmissionRef] = SubmissionRef.vformat
  implicit val format: OFormat[SdesSubmissionData] = Json.format
}
