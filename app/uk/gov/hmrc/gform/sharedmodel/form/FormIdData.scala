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

package uk.gov.hmrc.gform.sharedmodel.form

import cats.Show
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, UserId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

sealed trait FormIdData {

  val formTemplateId: FormTemplateId

  def fold[A](f: FormIdData.Plain => A)(g: FormIdData.WithAccessCode => A): A = this match {
    case p: FormIdData.Plain          => f(p)
    case w: FormIdData.WithAccessCode => g(w)
  }

  def toFormId: FormId = fold { p =>
    FormId(p.userId, p.formTemplateId)
  } { w =>
    FormId.fromAccessCode(w.userId, w.formTemplateId, w.accessCode)
  }

  def lowerCaseId: FormIdData = fold[FormIdData] { p =>
    p.copy(formTemplateId = FormTemplateId(p.formTemplateId.value.toLowerCase))
  } { w =>
    w.copy(formTemplateId = FormTemplateId(w.formTemplateId.value.toLowerCase))
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

  implicit val format: OFormat[FormIdData] = derived.oformat()

  implicit val show: Show[FormIdData] = Show.show(format.writes(_).toString)
}
