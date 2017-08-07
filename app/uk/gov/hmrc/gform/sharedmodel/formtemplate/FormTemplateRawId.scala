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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import play.api.libs.json._

object FormTemplateRawId {

  val writes: Writes[FormTemplateRawId] = Writes[FormTemplateRawId](id => JsString(id.value))
  val reads: Reads[FormTemplateRawId] = Reads[FormTemplateRawId] {
    case JsString(value) => JsSuccess(FormTemplateRawId(value))
    case otherwise => JsError(s"Invalid formTemplateId, expected JsString, got: $otherwise")
  }

  implicit val format: Format[FormTemplateRawId] = Format[FormTemplateRawId](reads, writes)

}

case class FormTemplateRawId(value: String)
