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

package uk.gov.hmrc.gform.sharedmodel.form

import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, SubmissionRef, UserId, ValueClassFormat }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

sealed trait FormIdData {

  val formTemplateId: FormTemplateId

  def toFormId: FormId = this match {
    case FormIdData.Plain(userId, formTemplateId) => FormId(userId, formTemplateId)
    case FormIdData.WithAccessCode(userId, formTemplateId, accessCode) =>
      FormId.fromAccessCode(userId, formTemplateId, accessCode)
  }
}

object FormIdData {

  case class Plain(userId: UserId, formTemplateId: FormTemplateId) extends FormIdData
  case class WithAccessCode(userId: UserId, formTemplateId: FormTemplateId, accessCode: AccessCode) extends FormIdData

  def fromForm(form: Form): Option[FormIdData] = {
    val formTemplateId = form.formTemplateId
    form._id.value.split(formTemplateId.value).toList match {
      case userId :: Nil               => Some(Plain(UserId(userId), formTemplateId))
      case userId :: accessCode :: Nil => Some(WithAccessCode(UserId(userId), formTemplateId, AccessCode(accessCode)))
      case _                           => None
    }
  }

  implicit val format: OFormat[FormIdData] = derived.oformat
}
