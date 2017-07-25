/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models

import play.api.libs.json.{ Format, JsError, JsString, JsSuccess, Reads, Writes }

case class FieldId(value: String) extends AnyVal {
  override def toString = value

  def withSuffix(suffix: String): FieldId = FieldId(value + "-" + suffix)

  def withJSSafeSuffix(suffix: String): FieldId = FieldId(value + "-" + suffix)
}

object FieldId {

  val writes = Writes[FieldId](id => JsString(id.value))
  val reads = Reads[FieldId] {
    case JsString(value) => JsSuccess(FieldId(value))
    case otherwise => JsError(s"Invalid fieldId, expected JsString, got: $otherwise")
  }

  implicit val format = Format[FieldId](reads, writes)
}
