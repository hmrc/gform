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

import cats.data.NonEmptyList
import play.api.libs.json.Reads._
import play.api.libs.json._
import JsonUtils.nelFormat

case class SuperFormTemplate(
  _id: FormTemplateId,
  title: String,
  description: String,
  superFormIdPrefix: String,
  superFormIdLabel: String,
  sections: NonEmptyList[SuperFormSection])

object SuperFormTemplate {

  val writes: OWrites[SuperFormTemplate] = OWrites[SuperFormTemplate](template => {
    import template._

    JsObject(
      Map[String, JsValue](
        "_id"               -> JsString(_id.value),
        "title"             -> JsString(title),
        "description"       -> JsString(description),
        "superFormIdPrefix" -> JsString(superFormIdPrefix),
        "superFormIdLabel"  -> JsString(superFormIdLabel),
        "sections"          -> JsArray(sections.map(jv => Json.toJson(jv)).toList)
      ))
  })

  val reads: Reads[SuperFormTemplate] = Reads[SuperFormTemplate] {
    case value: JsObject =>
      buildTemplate(value).fold[JsResult[SuperFormTemplate]](JsError(s"Expected json object"))(JsSuccess(_))
    case other => JsError(s"Expected json object but got $other")
  }

  def buildTemplate(value: JsObject): Option[SuperFormTemplate] =
    for {
      id          <- (value \ "_id").toOption
      title       <- (value \ "title").toOption
      description <- (value \ "description").toOption
      prefix      <- (value \ "superFormIdPrefix").toOption
      label       <- (value \ "superFormIdLabel").toOption
      sections    <- (value \ "sections").toOption
    } yield
      SuperFormTemplate(
        FormTemplateId(id.as[String]),
        title.as[String],
        description.as[String],
        prefix.as[String],
        label.as[String],
        sections.as[NonEmptyList[SuperFormSection]])

  implicit val format: OFormat[SuperFormTemplate] = OFormat[SuperFormTemplate](reads, writes)
}
