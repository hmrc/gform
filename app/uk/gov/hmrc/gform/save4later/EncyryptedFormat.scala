/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform.save4later

import play.api.libs.json._
import uk.gov.hmrc.crypto.{ Crypted, Decrypter, Encrypter, PlainText }

import scala.util.{ Failure, Success, Try }

object EncyryptedFormat {
  def formatEncrypted[A](jsonCrypto: Encrypter with Decrypter)(implicit format: Format[A]): Format[A] = new Format[A] {
    override def reads(json: JsValue): JsResult[A] = json match {
      case JsString(encrypted) =>
        Try(Json.parse(jsonCrypto.decrypt(Crypted(encrypted)).value).as[A]) match {
          case Success(f) => JsSuccess(f)
          case Failure(e) => JsError("Failed to parse JSON " + e)
        }
      case other => JsError("Failed to parse JSON. Invalid value " + other)
    }

    override def writes(o: A): JsValue = JsString(jsonCrypto.encrypt(PlainText(Json.toJson(o).toString())).value)
  }
}
