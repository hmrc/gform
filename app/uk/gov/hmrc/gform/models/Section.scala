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
import uk.gov.hmrc.gform.core.{ Invalid, Valid, ValidationResult, Opt }
import scala.collection.immutable.List

case class Section(
    title: String,
    shortName: Option[String],
    includeIf: Option[IncludeIf],
    fields: List[FieldValue]
) {
  private def atomicFields(fieldValues: List[FieldValue], data: Map[FieldId, FormField]): List[FieldValue] = {
    fieldValues.flatMap { (fieldValue) =>
      fieldValue.`type` match {
        case Group(gfvs, _, repMax, _, _, _) => atomicFields(fixLabels(gfvs), data) ++ findIdsInRepeatingGroup(gfvs, repMax, data)
        case fv @ _ => List(fieldValue)
      }
    }
  }

  def atomicFields(data: Map[FieldId, FormField]): List[FieldValue] = atomicFields(fields, data)

  private def findIdsInRepeatingGroup(fields: List[FieldValue], repeatsMax: Option[Int], data: Map[FieldId, FormField]): List[FieldValue] = {

    val result = if (data.isEmpty) {
      Nil // no data, no way of knowing if we have repeating groups
    } else {
      repeatsMax.map(extractRepeatingGroupFieldIds(fields, _, data)).getOrElse(Nil)
    }

    atomicFields(result, data)
  }

  private def extractRepeatingGroupFieldIds(fields: List[FieldValue], repeatsMax: Int, data: Map[FieldId, FormField]): List[FieldValue] = {
    (1 until repeatsMax).map { i =>
      fields.flatMap { fieldInGroup =>
        data.keys.flatMap { key =>
          val fieldName = s"${i}_${fieldInGroup.id.value}"
          key.value.startsWith(fieldName) match {
            case true => List(fieldInGroup.copy(
              id = FieldId(fieldName),
              label = buildRepeatingText(Some(fieldInGroup.label), i + 1).getOrElse(""),
              shortName = buildRepeatingText(fieldInGroup.shortName, i + 1)
            ))
            case false => Nil
          }
        }.toSet
      }
    }.toList.flatten
  }

  private def fixLabels(fieldValues: List[FieldValue]): List[FieldValue] = {
    fieldValues.map { field =>
      if (field.label.contains("$n") || (field.shortName.isDefined && field.shortName.get.contains("$n"))) {
        field.copy(
          label = buildRepeatingText(Some(field.label), 1).get,
          shortName = buildRepeatingText(field.shortName, 1)
        )
      } else {
        field
      }
    }
  }

  private def buildRepeatingText(text: Option[String], index: Int) = text match {
    case Some(txt) if text.get.contains("$n") => Some(txt.replace("$n", index.toString))
    case _ => text
  }
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
  fields: List[(List[FormField], FieldValue)]
)
