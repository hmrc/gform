/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.submission

import java.time.LocalDateTime

import play.api.libs.functional.syntax._
import play.api.libs.json.{ Json, OFormat, OWrites, Reads, _ }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

case class DmsMetaData(formTemplateId: FormTemplateId, customerId: String)

object DmsMetaData {
  implicit val format: OFormat[DmsMetaData] = Json.format[DmsMetaData]
}

case class Submission(
  _id: FormId,
  submittedDate: LocalDateTime,
  submissionRef: SubmissionRef,
  envelopeId: EnvelopeId,
  noOfAttachments: Int = 0,
  dmsMetaData: DmsMetaData)

object Submission {

  private val reads: Reads[Submission] = ((FormId.format: Reads[FormId]) and
    (JsPath \ "submittedDate").read[LocalDateTime] and
    SubmissionRef.oformat and
    EnvelopeId.format and
    (JsPath \ "attachment_count").read[Int] and
    DmsMetaData.format)(Submission.apply _)

  private val writes: OWrites[Submission] = OWrites[Submission](
    s =>
      FormId.format.writes(s._id) ++
        Json.obj("submittedDate" -> Writes.DefaultLocalDateTimeWrites.writes(s.submittedDate)) ++
        SubmissionRef.oformat.writes(s.submissionRef) ++
        EnvelopeId.format.writes(s.envelopeId) ++
        Json.obj("attachment_count" -> Writes.IntWrites.writes(s.noOfAttachments)) ++
        DmsMetaData.format.writes(s.dmsMetaData))

  implicit val format: OFormat[Submission] = OFormat[Submission](reads, writes)

}

case class PdfSummary(
  numberOfPages: Long,
  //TODO get rid of byte array and operate on streams or something similar
  pdfContent: Array[Byte])

case class PdfAndXmlSummaries(pdfSummary: PdfSummary, xmlSummary: Option[String] = None)
