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

package uk.gov.hmrc.gform.sharedmodel.history

import play.api.libs.json.{ Format, Json, OFormat }
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.gform.submission.WorkItemHistory

import java.time.Instant

case class WorkItemHistoryPageData(
  workItemHistory: List[WorkItemHistoryData],
  count: Long
)

object WorkItemHistoryPageData {
  implicit val format: OFormat[WorkItemHistoryPageData] = Json.format
}

case class WorkItemHistoryData(
  id: String,
  envelopeId: EnvelopeId,
  destinationId: DestinationId,
  formTemplateId: FormTemplateId,
  submissionRef: SubmissionRef,
  createdAt: Instant,
  responseStatus: Int,
  responseBody: String
)

object WorkItemHistoryData {
  def fromWorkItemHistory(workItemHistory: WorkItemHistory): WorkItemHistoryData =
    WorkItemHistoryData(
      workItemHistory._id.toString,
      workItemHistory.envelopeId,
      workItemHistory.destinationId,
      workItemHistory.formTemplateId,
      workItemHistory.submissionRef,
      workItemHistory.createdAt,
      workItemHistory.responseStatus,
      workItemHistory.responseBody
    )

  implicit val envelopeIdFormat: Format[EnvelopeId] = EnvelopeId.vformat
  implicit val formTemplateIdFormat: Format[FormTemplateId] = FormTemplateId.vformat
  implicit val submissionRefFormat: Format[SubmissionRef] = SubmissionRef.vformat
  implicit val destinationIdFormat: Format[DestinationId] = DestinationId.format
  implicit val format: OFormat[WorkItemHistoryData] = Json.format
}
