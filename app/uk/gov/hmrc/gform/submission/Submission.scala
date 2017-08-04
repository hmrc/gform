/*
 * Copyright 2017 HM Revenue & Customs
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

import play.api.libs.json.Json
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

case class DmsMetaData(
  formTypeId: FormTemplateId
)

object DmsMetaData {
  implicit val format = Json.format[DmsMetaData]
}

case class Submission(
  _id: FormId,
  submittedDate: LocalDateTime,
  submissionRef: SubmissionRef,
  envelopeId: EnvelopeId,
  dmsMetaData: DmsMetaData
)

object Submission {
  implicit val format = Json.format[Submission]
}

case class PdfSummary(
  numberOfPages: Long,
  //TODO get rid of byte array and operate on streams or something similar
  pdfContent: Array[Byte]
)

case class SubmissionAndPdf(
  submission: Submission,
  pdfSummary: PdfSummary
)
