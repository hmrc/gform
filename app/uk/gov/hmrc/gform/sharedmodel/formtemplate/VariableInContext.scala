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

case class VariableInContext(field: String)

object VariableInContext {

  implicit val format: OFormat[VariableInContext] = {
    val writes: OWrites[VariableInContext] = Json.writes[VariableInContext]
    val reads: Reads[VariableInContext] = Reads { json =>
      json.validate[String] match {
        case JsSuccess(string, _) => JsSuccess(VariableInContext(string))
        case JsError(_) => (json \ "field").validate[VariableInContext]
      }
    }
    OFormat[VariableInContext](reads, writes)
  }
}
