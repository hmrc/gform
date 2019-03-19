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

package uk.gov.hmrc.gform.sharedmodel.formtemplate
import play.api.libs.json._

case class ChatRoomId(value: String) extends AnyVal

case class TemplateName(value: String) extends AnyVal

case class WebChat(chatRoomId: ChatRoomId, templateName: Option[TemplateName])

case object WebChat {

  implicit val format: OFormat[WebChat] = {

    val reads: Reads[WebChat] = for {
      chatRoomId <- (JsPath \ "chatRoomId").read[String]
      templateName <- (JsPath \ "templateName")
                       .readNullable[String]
                       .map(_.fold(Some(TemplateName("hmrc7")))(value => Some(TemplateName(value))))

    } yield WebChat(ChatRoomId(chatRoomId), templateName)

    val writes: OWrites[WebChat] = OWrites {
      case WebChat(ChatRoomId(chatRoomId), Some(TemplateName(templateName))) =>
        JsObject(Seq(("chatRoomId", JsString(chatRoomId)), ("templateName", JsString(templateName))))
      case WebChat(ChatRoomId(chatRoomId), None) =>
        JsObject(Seq(("chatRoomId", JsString(chatRoomId)), ("templateName", JsString("hmrc7"))))
    }

    OFormat(reads, writes)
  }

}
