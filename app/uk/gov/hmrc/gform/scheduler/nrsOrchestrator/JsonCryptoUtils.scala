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

import play.api.libs.json.{ JsDefined, JsError, JsResult, JsString, JsSuccess, JsUndefined, JsValue, Json, Reads, Writes }
import uk.gov.hmrc.crypto.{ Crypted, Decrypter, Encrypter, PlainText }

import reflect.runtime.universe._

object JsonCryptoUtils {
  def encrypt[T: TypeTag](obj: T)(implicit writes: Writes[T], jsonCrypto: Encrypter): JsString =
    if (typeOf[T] <:< typeOf[String]) {
      JsString(jsonCrypto.encrypt(PlainText(obj.toString)).value)
    } else {
      JsString(jsonCrypto.encrypt(PlainText(Json.stringify(Json.toJson(obj)))).value)
    }

  def decrypt[T: TypeTag](json: JsValue, fieldName: String)(implicit
    reads: Reads[T],
    jsonCrypto: Decrypter
  ): JsResult[T] =
    json \ fieldName match {
      case JsDefined(value) =>
        value
          .validate[String]
          .flatMap { field =>
            val decryptedValue = jsonCrypto.decrypt(Crypted(field)).value
            if (typeOf[T] <:< typeOf[String]) {
              JsSuccess(decryptedValue.asInstanceOf[T])
            } else {
              Json.parse(jsonCrypto.decrypt(Crypted(field)).value).validate[T]
            }

          }
      case undefined: JsUndefined => JsError(s"$fieldName not found")
    }
}
