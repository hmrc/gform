/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.formmetadata

import java.time.Instant

import cats.Show
import play.api.libs.json.{ Json, OFormat }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, SubmissionRef, UserId }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormId, FormIdData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

case class FormMetadata(
  _id: FormId,
  userId: UserId,
  formTemplateId: FormTemplateId,
  submissionRef: Option[SubmissionRef],
  parentFormSubmissionRefs: List[SubmissionRef],
  createdAt: Instant,
  updatedAt: Instant
) {
  def formIdData: FormIdData = submissionRef match {
    case None     => FormIdData.Plain(userId, formTemplateId)
    case Some(sr) => FormIdData.WithAccessCode(userId, formTemplateId, AccessCode.fromSubmissionRef(sr))
  }
}

object FormMetadata {
  implicit val format: OFormat[FormMetadata] = {
    implicit val formIdFormat = FormId.vformat
    implicit val userIdFormat = UserId.vformat
    implicit val submissionRefFormat = SubmissionRef.vformat
    Json.format
  }

  implicit val show: Show[FormMetadata] = Show.show(format.writes(_).toString)
}
