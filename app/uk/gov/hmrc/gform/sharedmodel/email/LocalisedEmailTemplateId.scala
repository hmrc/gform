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

package uk.gov.hmrc.gform.sharedmodel.email

import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.{ EmailVerifierService, LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierTemplateId

case class LocalisedEmailTemplateId(emailTemplateId: String, emailTemplateIdCy: Option[String]) {
  def toDigitalContact =
    EmailVerifierService.digitalContact(
      EmailTemplateId(emailTemplateId),
      emailTemplateIdCy.map(EmailTemplateId.apply)
    )

  def toNotify =
    EmailVerifierService.notify(
      NotifierTemplateId(emailTemplateId),
      emailTemplateIdCy.map(NotifierTemplateId.apply)
    )

  def getEmailTemplateId(lang: LangADT) =
    lang match {
      case LangADT.En => emailTemplateId
      case LangADT.Cy => emailTemplateIdCy.getOrElse(emailTemplateId)
    }
}

object LocalisedEmailTemplateId {
  implicit val reads: Reads[LocalisedEmailTemplateId] = Reads {
    case JsString(emailTemplateId) => JsSuccess(LocalisedEmailTemplateId(emailTemplateId, None))
    case obj @ JsObject(_) =>
      obj.validate[LocalisedString].flatMap { localisedString =>
        localisedString.m.get(LangADT.En) match {
          case Some(emailTemplateId) =>
            JsSuccess(LocalisedEmailTemplateId(emailTemplateId, localisedString.m.get(LangADT.Cy)))
          case None => JsError("Invalid email template id definition. Missing 'en' field with english template id")
        }
      }
    case otherwise =>
      JsError("Invalid email template id definition. Expected json String or json Object, but got: " + otherwise)
  }

  private val writes: OWrites[LocalisedEmailTemplateId] = Json.format[LocalisedEmailTemplateId]

  implicit val format: OFormat[LocalisedEmailTemplateId] = OFormat(reads, writes)
}
