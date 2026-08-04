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

package uk.gov.hmrc.gform.sharedmodel.sdes

import io.circe.Printer
import julienrf.json.derived
import play.api.libs.json.{ Format, OFormat }
import uk.gov.hmrc.gform.scheduler.TraceableWorkItem
import uk.gov.hmrc.gform.scheduler.asynchandlebars.AsyncHandlebarsWorkItem
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.mongo.workitem.WorkItem

case class AsyncWorkItemData(
  id: String,
  envelopeId: EnvelopeId,
  destinationId: String,
  formTemplateId: FormTemplateId,
  submissionRef: SubmissionRef,
  uri: String,
  method: String,
  payload: String,
  username: Option[String]
)

object AsyncWorkItemData {
  implicit val envelopeIdFormat: Format[EnvelopeId] = EnvelopeId.vformat
  implicit val submissionRefFormat: Format[SubmissionRef] = SubmissionRef.vformat
  implicit val format: OFormat[AsyncWorkItemData] = derived.oformat()

  private val printer = Printer.spaces2
    .copy(
      colonLeft = "",
      lrbracketsEmpty = ""
    )

  def fromAsyncWorkItem(
    workItem: WorkItem[TraceableWorkItem[AsyncHandlebarsWorkItem]]
  ): AsyncWorkItemData =
    AsyncWorkItemData(
      workItem.id.toString,
      workItem.item.envelopeId,
      workItem.item.destinationId.id,
      workItem.item.formTemplateId,
      workItem.item.submissionRef,
      workItem.item.data.uri,
      workItem.item.data.method.toString,
      io.circe.parser.parse(workItem.item.data.payload).toOption.fold(workItem.item.data.payload)(printer.print),
      None
    )
}
