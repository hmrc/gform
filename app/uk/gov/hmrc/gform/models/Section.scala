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
import uk.gov.hmrc.gform.core.{ Invalid, Valid, ValidationResult }

import scala.collection.immutable.List

case class Section(
    title: String,
    fields: List[FieldValue]
) {
  private def atomicFields(fieldValues: List[FieldValue]): List[FieldValue] = {
    fieldValues.flatMap { (fieldValue) =>
      fieldValue.`type` match {
        case Group(gfvs, _) => atomicFields(gfvs)
        case fv @ _ => List(fieldValue)
      }
    }
  }
  def atomicFields: List[FieldValue] = atomicFields(fields)
}

object Section {
  implicit val format = Json.format[Section]

  def validateUniqueFields(sectionsList: List[Section]): ValidationResult = {
    val fieldIds: List[FieldId] = sectionsList.flatMap(_.fields.map(_.id))
    val duplicates: List[FieldId] = fieldIds.groupBy(identity).collect { case (fId, List(_, _, _*)) => fId }.toList

    duplicates.isEmpty match {
      case true => Valid
      case false => Invalid(s"Some FieldIds are defined more than once: ${duplicates.map(_.value)}")
    }
  }

  def validateChoiceHelpText(sectionsList: List[Section]): ValidationResult = {
    val choiceFieldIdMap: Map[FieldId, Boolean] = sectionsList.flatMap(_.fields).map(fv => (fv.id, fv.`type`))
      .collect {
        case (fId, Choice(_, options, _, _, helpTextList @ Some(x :: xs))) =>
          (fId, options.toList.size.equals(helpTextList.getOrElse(List.empty).size))
      }
      .toMap

    val choiceFieldIdResult = choiceFieldIdMap.filter(value => value._2.equals(false))

    choiceFieldIdResult.isEmpty match {
      case true => Valid
      case false => Invalid(s"Choice components doesn't have equal number of choices and help texts ${choiceFieldIdResult.keys.toList}")
    }
  }

  /*
   * The Following Function validates that FieldIds contained in format in Date FieldId,
   * must exist and must correspond to Date Fields
   */
  /* def validateFieldIdInDate(sectionsList: List[Section]): ValidationResult = {
    val fieldIdDateConstraints: Map[FieldId, List[String]] = sectionsList.flatMap(_.fields).map(fv => (fv.id, fv.`type`))
      .collect { case (fId, Date(DateConstraints(constrList), _, _)) => (fId, constrList) }
      .collect { case (fId, List(DateConstraint(_, words @ AnyWord(_), _))) => (fId, List(words.value)) }
      .toMap

    val fieldIdsList: List[String] = sectionsList.flatMap(_.fields).map(fv => (fv.id, fv.`type`))
      .collect { case (fId, Date(_, _, _)) => fId.value }

    // each FieldId in AnyWord should be contained in list of FieldIds
    val fieldIdPairs: Map[FieldId, Boolean] = fieldIdDateConstraints.mapValues { words =>
      val resultList = words.map { word =>
        fieldIdsList.contains(word)
      }

      !resultList.contains(false)
    }

    val fieldIdResult = fieldIdPairs.filter(value => value._2.equals(false))

    fieldIdResult.isEmpty match {
      case true => Valid
      case false => Invalid(s"Some FieldIds are defined in Dates and either they don't exist" +
        s" or they don't belong to Date Field types ${fieldIdResult.keys.toList}")
    }
  }*/

}

case class SectionFormField(
  title: String,
  fields: List[(FormField, FieldValue)]
)
