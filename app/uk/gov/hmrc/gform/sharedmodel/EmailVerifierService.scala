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

package uk.gov.hmrc.gform.sharedmodel

import cats.Show
import cats.syntax.all._
import play.api.libs.json.{ JsDefined, JsError, JsString, JsSuccess, JsUndefined, OFormat, Reads }
import uk.gov.hmrc.gform.sharedmodel.email.EmailTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.OFormatWithTemplateReadFallback
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierTemplateId

sealed trait EmailVerifierService extends Product with Serializable

object EmailVerifierService {

  def notify(emailTemplateId: NotifierTemplateId, emailTemplateIdCy: Option[NotifierTemplateId]) =
    Notify(emailTemplateId, emailTemplateIdCy)
  def digitalContact(emailTemplateId: EmailTemplateId, emailTemplateIdCy: Option[EmailTemplateId]) =
    DigitalContact(emailTemplateId, emailTemplateIdCy)

  case class Notify(emailTemplateId: NotifierTemplateId, emailTemplateIdCy: Option[NotifierTemplateId])
      extends EmailVerifierService {
    def notifierTemplateId(l: LangADT): NotifierTemplateId = l match {
      case LangADT.En => emailTemplateId
      case LangADT.Cy => emailTemplateIdCy.getOrElse(emailTemplateId)
    }
  }
  case class DigitalContact(emailTemplateId: EmailTemplateId, emailTemplateIdCy: Option[EmailTemplateId])
      extends EmailVerifierService {
    def emailTemplateId(l: LangADT): EmailTemplateId = l match {
      case LangADT.En => emailTemplateId
      case LangADT.Cy => emailTemplateIdCy.getOrElse(emailTemplateId)
    }
  }

  private val templateReads: Reads[EmailVerifierService] = Reads { json =>
    ((json \ "service"), (json \ "emailTemplateId")) match {
      case (JsDefined(JsString("notify")), JsDefined(JsString(emailTemplateId))) =>
        JsSuccess(notify(NotifierTemplateId(emailTemplateId), None))
      case (JsDefined(JsString("digitalContact")), JsDefined(JsString(emailTemplateId))) =>
        JsSuccess(digitalContact(EmailTemplateId(emailTemplateId), None))
      case (JsDefined(_), JsUndefined()) => JsError(s"Missing field 'emailTemplateId' in json: $json")
      case (JsDefined(unknown), _) =>
        JsError(s"Unsupported email service '$unknown'. Only 'notify' or 'digitalContact' are supported")
      case (JsUndefined(), _) => JsError(s"Missing email service. Specify one of 'notify' or 'digitalContact'")
    }
  }

  implicit val format: OFormat[EmailVerifierService] = OFormatWithTemplateReadFallback(templateReads)

  implicit val show: Show[EmailVerifierService] = Show.show {
    case EmailVerifierService.Notify(emailTemplateId, Some(emailTemplateIdCy)) =>
      show"""{"en": "$emailTemplateId", "cy": "$emailTemplateIdCy"}"""
    case EmailVerifierService.Notify(emailTemplateId, None) => show"$emailTemplateId"
    case EmailVerifierService.DigitalContact(emailTemplateId, Some(emailTemplateIdCy)) =>
      show"""{"en": "$emailTemplateId", "cy": "$emailTemplateIdCy"}"""
    case EmailVerifierService.DigitalContact(emailTemplateId, None) => show"$emailTemplateId"
  }

}
