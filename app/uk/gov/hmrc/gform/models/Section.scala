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

import play.api.libs.json.Json
import uk.gov.hmrc.gform.core.{ Valid, Invalid, ValidationResult }

import scala.collection.immutable.List

case class Section(
  title: String,
  fields: List[FieldValue]
)

object Section {
  implicit val format = Json.format[Section]

  def validate(sectionsList: List[Section]): ValidationResult = {
    val fieldIds: List[FieldId] = sectionsList.flatMap(_.fields.map(_.id))
    val duplicates: List[FieldId] = fieldIds.groupBy(identity).collect { case (fId, List(_, _, _*)) => fId }.toList

    duplicates.isEmpty match {
      case true => Valid
      case false => Invalid(s"Some FieldIds are defined more than once: ${duplicates.map(_.value)}")
    }
  }

}

case class SectionFormField(
  title: String,
  fields: List[(FormField, FieldValue)]
)
