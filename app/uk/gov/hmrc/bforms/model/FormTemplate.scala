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

package uk.gov.hmrc.bforms.model

import play.api.libs.json.{ Format, JsError, JsSuccess, Reads, Writes, JsObject }

case class FormTemplate(value: JsObject) extends AnyVal

object FormTemplate {
  val writes = Writes[FormTemplate](id => id.value)
  val reads = Reads[FormTemplate] {
    case o @ JsObject(_) => JsSuccess(FormTemplate(o))
    case otherwise => JsError(s"Invalid FormTemplate, expected JsObject, got: $otherwise")
  }

  implicit val format = Format[FormTemplate](reads, writes)
}
