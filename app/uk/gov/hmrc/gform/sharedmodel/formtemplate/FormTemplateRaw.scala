/*
 * Copyright 2019 HM Revenue & Customs
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

case class FormTemplateRaw(value: JsObject) {
  def _id: FormTemplateRawId = {

    val id = (value \ "_id").toOption match {
      case None               => throw new NoSuchElementException("There is element at '_id' path")
      case Some(JsString(id)) => id
      case Some(other) =>
        throw new NoSuchElementException(s"Element at '_id' path is not a string and it contained '$other'")
    }

    FormTemplateRawId(id)
  }
}

object FormTemplateRaw {

  val writes: OWrites[FormTemplateRaw] = OWrites[FormTemplateRaw](ftr => ftr.value)

  val reads: Reads[FormTemplateRaw] = Reads[FormTemplateRaw] {
    case value: JsObject => JsSuccess(FormTemplateRaw(value))
    case other           => JsError(s"Expected json object but got $other")
  }

  implicit val format: OFormat[FormTemplateRaw] = OFormat[FormTemplateRaw](reads, writes)

}
