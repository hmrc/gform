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
import uk.gov.hmrc.mongo.workitem.{ ProcessingStatus, WorkItem }

import java.time.Instant

case class SdesWorkItemPageData(
  sdesWorkItem: List[SdesWorkItemData],
  count: Long
)

object SdesWorkItemPageData {
  implicit val format: OFormat[SdesWorkItemPageData] = Json.format
}

case class SdesWorkItemData(
  id: String,
  envelopeId: EnvelopeId,
  destination: SdesDestination,
  formTemplateId: FormTemplateId,
  submissionRef: SubmissionRef,
  numberOfFiles: Int,
  status: ProcessingStatus,
  failureCount: Int,
  receivedAt: Instant,
  updatedAt: Instant
)

object SdesWorkItemData {

  def fromWorkItem(workItem: WorkItem[SdesWorkItem], numberOfFiles: Int = 0) = SdesWorkItemData(
    workItem.id.toString,
    workItem.item.envelopeId,
    workItem.item.destination,
    workItem.item.formTemplateId,
    workItem.item.submissionRef,
    numberOfFiles,
    workItem.status,
    workItem.failureCount,
    workItem.receivedAt,
    workItem.updatedAt
  )

  implicit val envelopeIdFormat: Format[EnvelopeId] = EnvelopeId.vformat
  implicit val formTemplateIdFormat: Format[FormTemplateId] = FormTemplateId.vformat
  implicit val submissionRefFormat: Format[SubmissionRef] = SubmissionRef.vformat
  implicit val processingStatus: Format[ProcessingStatus] = ProcessingStatus.format
  implicit val format: OFormat[SdesWorkItemData] = Json.format
}
