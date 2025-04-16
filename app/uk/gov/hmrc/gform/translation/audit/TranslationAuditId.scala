/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.translation.audit

import cats.Eq
import org.bson.types.ObjectId
import play.api.libs.json.{ Reads, Writes }
import play.api.libs.json._

final case class TranslationAuditId(id: String) {
  def toObjectId: ObjectId = new ObjectId(id)
}

object TranslationAuditId {

  implicit val equal: Eq[TranslationAuditId] = Eq.fromUniversalEquals

  implicit val bsonRead: Reads[TranslationAuditId] =
    (__ \ "$oid").read[String].map { bsonId =>
      new TranslationAuditId(bsonId)
    }

  implicit val bsonReadWrite: Writes[TranslationAuditId] = new Writes[TranslationAuditId] {
    def writes(bsonId: TranslationAuditId): JsValue = JsString(bsonId.id)
  }

  val flatReads: Reads[TranslationAuditId] = Reads[TranslationAuditId] {
    case JsString(value) => JsSuccess(TranslationAuditId(value))
    case otherwise       => JsError(s"Invalid translationAuditId, expected JsString, got: $otherwise")
  }

}
