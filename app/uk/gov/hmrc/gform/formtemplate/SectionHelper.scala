/*
 * Copyright 2020 HM Revenue & Customs
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

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.sharedmodel.LabelHelper
import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.collection.immutable.List

object SectionHelper {
  def pages(section: Section): NonEmptyList[Page] =
    section match {
      case s: Section.NonRepeatingPage => NonEmptyList.of(s.page)
      case s: Section.RepeatingPage    => NonEmptyList.of(s.page)
      case s: Section.AddToList        => s.pages
    }

  def pages(sections: List[Section]): List[Page] =
    sections.flatMap(pages(_).toList)

  def atomicLeafFields(section: Section, data: Map[FormComponentId, FormField]): List[FormComponent] =
    atomicLeafFields(listTopLevelFormComponents(section), data)

  // The FormComponents returned include those with ContainerComponent `type` (e.g. Group).
  // We don't drill down into the ContainerComponents to get at the leaves in this method
  private def listTopLevelFormComponents(section: Section): List[FormComponent] =
    pages(section).toList.flatMap(_.fields)

  def atomicLeafFields(
    formComponents: List[FormComponent],
    data: Map[FormComponentId, FormField]): List[FormComponent] =
    formComponents.flatMap { formComponent =>
      formComponent.`type` match {
        case _: Text | _: TextArea | _: Choice | _: HmrcTaxPeriod | _: FileUpload | _: Date | _: Address |
            _: UkSortCode =>
          List(formComponent)
        case Group(gfvs, _, repMax, _, _, _) =>
          atomicLeafFields(fixLabels(gfvs), data) ++ findIdsInRepeatingGroup(gfvs, repMax, data)
        case RevealingChoice(opts, _) =>
          formComponent :: atomicLeafFields(opts.toList.flatMap(_.revealingFields), data)
        case _: InformationMessage => Nil
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

    atomicLeafFields(result, data)
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
                    label = LabelHelper.buildRepeatingLabel(fieldInGroup.label, i + 1),
                    shortName = LabelHelper.buildRepeatingLabel(fieldInGroup.shortName, i + 1)
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
          label = LabelHelper.buildRepeatingLabel(field.label, 1),
          shortName = LabelHelper.buildRepeatingLabel(field.shortName, 1)
        )
      }
    }
}
