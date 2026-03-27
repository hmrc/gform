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

package uk.gov.hmrc.gform.scheduler.nrsOrchestrator

import play.api.libs.json.{ JsObject, JsResult, JsValue, Json, OFormat, Reads, __ }
import uk.gov.hmrc.crypto.{ Crypted, Decrypter, Encrypter, PlainText }

object JsonCryptoUtils {
  def formatEncrypted[T](format: OFormat[T])(implicit jsonCrypto: Encrypter with Decrypter): OFormat[T] =
    new OFormat[T] {
      override def writes(o: T): JsObject =
        Json.obj(
          "data" -> jsonCrypto.encrypt(PlainText(Json.toJson(o)(format).toString())).value
        )

      val reads: Reads[T] = (__ \ "data")
        .read[String]
        .map(s => Json.parse(jsonCrypto.decrypt(Crypted(s)).value).as[T](format))

      override def reads(json: JsValue): JsResult[T] =
        json.validate[T](reads)
    }
}
