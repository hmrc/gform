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

package uk.gov.hmrc.gform.formtemplate

import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.collection.immutable.List

object SectionHelper {

  def atomicFields(section: BaseSection, data: Map[FormComponentId, FormField]): List[FormComponent] =
    atomicFields(section.fields, data)

  private def atomicFields(
    fieldValues: List[FormComponent],
    data: Map[FormComponentId, FormField]): List[FormComponent] =
    fieldValues.flatMap { (fieldValue) =>
      fieldValue.`type` match {
        case Group(gfvs, _, repMax, _, _, _) =>
          atomicFields(fixLabels(gfvs), data) ++ findIdsInRepeatingGroup(gfvs, repMax, data)
        case fv @ _ => List(fieldValue)
      }
    }

  private def findIdsInRepeatingGroup(
    fields: List[FormComponent],
    repeatsMax: Option[Int],
    data: Map[FormComponentId, FormField]): List[FormComponent] = {

    val result = if (data.isEmpty) {
      Nil // no data, no way of knowing if we have repeating groups
    } else {
      repeatsMax.map(extractRepeatingGroupFieldIds(fields, _, data)).getOrElse(Nil)
    }

    atomicFields(result, data)
  }

  private def extractRepeatingGroupFieldIds(
    fields: List[FormComponent],
    repeatsMax: Int,
    data: Map[FormComponentId, FormField]): List[FormComponent] =
    (1 until repeatsMax)
      .map { i =>
        fields.flatMap { fieldInGroup =>
          data.keys.flatMap { key =>
            val fieldName = s"${i}_${fieldInGroup.id.value}"
            key.value.startsWith(fieldName) match {
              case true =>
                List(
                  fieldInGroup.copy(
                    id = FormComponentId(fieldName),
                    label = LocalisedString(labelRepeatingGroupComponents(fieldInGroup.label.m, i + 1)),
                    shortName = fieldInGroup.shortName.map(ls=> LocalisedString(labelRepeatingGroupComponents(ls.m,i+1)))
                  ))
              case false => Nil
            }
          }.toSet
        }
      }
      .toList
      .flatten

  private def fixLabels(fieldValues: List[FormComponent]): List[FormComponent] =
    fieldValues.map { field =>
      {
        field.copy(
          label = LocalisedString(labelRepeatingGroupComponents(field.label.m, 1)),
          shortName = field.shortName.map(ls=> LocalisedString(labelRepeatingGroupComponents(ls.m,1)))
        )
      }
    }

  private def buildRepeatingText(text: String, index: Int) =
    text.replace("$n", index.toString)

  def labelRepeatingGroupComponents(fieldInComponent: Map[LangADT, String], index: Int) =
    fieldInComponent.map {
      case (langADT, message) => (langADT, buildRepeatingText(message, index))
    }
}
