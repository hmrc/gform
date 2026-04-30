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

package uk.gov.hmrc.gform.scheduler.asynchandlebars

import play.api.libs.json._
import uk.gov.hmrc.crypto.{ Crypted, Decrypter, Encrypter, PlainText }
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ HttpMethod, ProfileName }

case class AsyncHandlebarsWorkItem(
  profile: ProfileName,
  uri: String,
  method: HttpMethod,
  contentType: ContentType,
  payload: String
)

object AsyncHandlebarsWorkItem {
  def formatEncrypted(jsonCrypto: Encrypter with Decrypter): OFormat[AsyncHandlebarsWorkItem] =
    new OFormat[AsyncHandlebarsWorkItem] {
      private val uri = "uri"
      private val method = "method"
      private val payload = "payload"

      override def writes(workItem: AsyncHandlebarsWorkItem): JsObject =
        ProfileName.oformat.writes(workItem.profile) ++
          Json.obj(uri -> workItem.uri) ++
          Json.obj(method -> workItem.method) ++
          ContentType.oformat.writes(workItem.contentType) ++
          Json.obj(payload -> JsString(jsonCrypto.encrypt(PlainText(workItem.payload)).value))

      override def reads(json: JsValue): JsResult[AsyncHandlebarsWorkItem] =
        for {
          profile     <- ProfileName.oformat.reads(json)
          uri         <- (json \ uri).validate[String]
          method      <- (json \ method).validate[HttpMethod]
          contentType <- ContentType.oformat.reads(json)
          payload     <- (json \ payload).validate[String].map(payload => jsonCrypto.decrypt(Crypted(payload)).value)
        } yield AsyncHandlebarsWorkItem(
          profile,
          uri,
          method,
          contentType,
          payload
        )
    }
}
