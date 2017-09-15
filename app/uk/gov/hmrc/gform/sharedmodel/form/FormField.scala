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

package uk.gov.hmrc.gform.sharedmodel.form

import play.api.libs.functional.syntax._
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

case class FormField(id: FormComponentId, value: String)

object FormField {

  implicit val reads: Reads[FormField] = (
    (FormComponentId.oformat: Reads[FormComponentId]) and
    (JsPath \ "value").read[String]
  )(FormField.apply _)

  implicit val writes = OWrites[FormField] { formField =>

    Json.obj("id" -> formField.id) ++
      Json.obj("value" -> formField.value)
  }

  implicit val format: OFormat[FormField] = OFormat(reads, writes)
}
