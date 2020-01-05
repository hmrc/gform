/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.form

import cats.{ Eq, Show }
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, SubmissionRef, UserId, ValueClassFormat }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

case class FormId(value: String)

object FormId {

  def apply(userId: UserId, formTemplateId: FormTemplateId): FormId =
    new FormId(s"${userId.value}-${formTemplateId.value}")

  def fromAccessCode(userId: UserId, formTemplateId: FormTemplateId, accessCode: AccessCode): FormId =
    new FormId(s"${userId.value}-${formTemplateId.value}-${accessCode.value}")

  def fromSubmissionRef(userId: UserId, formTemplateId: FormTemplateId, submissionRef: SubmissionRef): FormId =
    new FormId(s"${userId.value}-${formTemplateId.value}-${submissionRef.value}")

  implicit val format: OFormat[FormId] = ValueClassFormat.oformat("_id", FormId.apply, _.value)

  val vformat: Format[FormId] = ValueClassFormat.vformat("_id", FormId.apply, x => JsString(x.value))

  implicit val equal: Eq[FormId] = Eq.fromUniversalEquals[FormId]

  implicit val show: Show[FormId] = Show.show(_.value)

}
