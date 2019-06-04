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
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.gform.formtemplate.SectionHelper.labelRepeatingGroupComponents
import uk.gov.hmrc.gform.sharedmodel.LocalisedString

import scala.collection.immutable.List

sealed trait BaseSection {
  def title: LocalisedString
  def shortName: Option[LocalisedString]
  def fields: List[FormComponent]
}

case class ExpandedSection(expandedFormComponents: List[ExpandedFormComponent]) extends AnyVal {
  def toExpandedFormTemplate: ExpandedFormTemplate = ExpandedFormTemplate(this :: Nil)
}

case class Section(
  title: LocalisedString,
  description: Option[LocalisedString],
  progressIndicator: Option[LocalisedString] = None,
  shortName: Option[LocalisedString],
  includeIf: Option[IncludeIf],
  repeatsMax: Option[TextExpression],
  repeatsMin: Option[TextExpression],
  validators: Option[Validator],
  fields: List[FormComponent],
  continueLabel: Option[LocalisedString],
  continueIf: Option[ContinueIf]
) extends BaseSection {

  val expandSection: ExpandedSection = ExpandedSection(fields.map(_.expandFormComponent)) // TODO expand sections

  private def atomicFields(
    fieldValues: List[FormComponent],
    data: Map[FormComponentId, FormField]): List[FormComponent] =
    fieldValues.flatMap { fieldValue =>
      fieldValue.`type` match {
        case Group(gfvs, _, repMax, _, _, _) =>
          atomicFields(fixLabels(gfvs), data) ++ findIdsInRepeatingGroup(gfvs, repMax, data)
        case fv @ _ => List(fieldValue)
      }
    }

  def atomicFields(data: Map[FormComponentId, FormField]): List[FormComponent] = atomicFields(fields, data)

  def isRepeating: Boolean = repeatsMax.isDefined && repeatsMin.isDefined

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
                    shortName =
                      fieldInGroup.shortName.map(ls => LocalisedString(labelRepeatingGroupComponents(ls.m, i + 1)))
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
      field.copy(
        label = LocalisedString(labelRepeatingGroupComponents(field.label.m, 1)),
        shortName = field.shortName.map(ls => LocalisedString(labelRepeatingGroupComponents(ls.m, 1)))
      )
    }
}

object Section {
  implicit val format: OFormat[Section] = Json.format[Section]
}

case class DeclarationSection(
  title: LocalisedString,
  description: Option[LocalisedString],
  shortName: Option[LocalisedString],
  fields: List[FormComponent]
) extends BaseSection

object DeclarationSection {
  implicit val format: OFormat[DeclarationSection] = Json.format[DeclarationSection]
}

case class AcknowledgementSection(
  title: LocalisedString,
  description: Option[LocalisedString],
  shortName: Option[LocalisedString],
  fields: List[FormComponent]
) extends BaseSection

object AcknowledgementSection {
  implicit val format: OFormat[AcknowledgementSection] = Json.format[AcknowledgementSection]
}

case class EnrolmentSection(
  title: LocalisedString,
  shortName: Option[LocalisedString],
  fields: List[FormComponent],
  identifiers: NonEmptyList[IdentifierRecipe],
  verifiers: List[VerifierRecipe]
) extends BaseSection

object EnrolmentSection {
  import JsonUtils._
  implicit val format: OFormat[EnrolmentSection] = Json.format[EnrolmentSection]
}

case class SectionFormField(title: LocalisedString, fields: List[(List[FormField], FormComponent)])

case class IdentifierRecipe(key: String, value: FormCtx)
object IdentifierRecipe {
  implicit val format: OFormat[IdentifierRecipe] = Json.format[IdentifierRecipe]
}

case class VerifierRecipe(key: String, value: FormCtx)
object VerifierRecipe {
  implicit val format: OFormat[VerifierRecipe] = Json.format[VerifierRecipe]
}
