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

package uk.gov.hmrc.gform.structuredform

import cats.data.NonEmptyList
import cats.instances.option._
import cats.syntax.option._
import uk.gov.hmrc.gform.formtemplate.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.JsonNodes._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.structuredform.StructuredFormValue.{ ArrayNode, ObjectStructure, TextNode }

object StructuredFormDataBuilder {
  def apply(form: Form, template: FormTemplate): ObjectStructure =
    StructuredFormValue.ObjectStructure(new StructuredFormDataBuilder(form, template).build())
}

class StructuredFormDataBuilder(form: Form, template: FormTemplate) {
  private val formValuesByUnindexedId: Map[FormComponentId, NonEmptyList[String]] =
    form.formData.fields
      .groupBy(f => RepeatingComponentService.reduceToTemplateFieldId(f.id))
      .mapValues(values => NonEmptyList.fromListUnsafe(values.map(_.value).toList))

  private val multiChoiceFieldIds: Set[FormComponentId] = extractMultiChoiceFieldIds(template)

  def build(): List[Field] =
    buildSections ++ buildBaseSection(template.acknowledgementSection) ++ buildBaseSection(template.declarationSection)

  private def buildSections(): List[Field] =
    for {
      section     <- template.sections
      field       <- section.fields
      fieldAsJson <- buildField(field, RepeatingComponentService.isRepeatingSection(section))
    } yield fieldAsJson

  def buildBaseSection(section: BaseSection): List[Field] =
    for {
      unstructuredField <- section.fields
      structuredField   <- buildField(unstructuredField, false)
    } yield structuredField

  private def buildField(field: FormComponent, repeatable: Boolean): Seq[Field] =
    field.`type` match {
      case g: Group => buildGroupFields(g)
      case _        => buildNonGroupField(field, repeatable).toSeq
    }

  private def buildGroupFields(group: Group): Seq[Field] =
    group.fields.flatMap { buildNonGroupField(_, repeatable = true) }

  private def buildNonGroupField(nonGroupField: FormComponent, repeatable: Boolean): Option[Field] =
    nonGroupField.`type` match {
      case mf: MultiField => buildComposite(nonGroupField, mf, repeatable)
      case _ =>
        formValuesByUnindexedId.get(nonGroupField.id).map {
          buildSimpleField(nonGroupField, _, repeatable, multiChoiceFieldIds.contains(nonGroupField.id))
        }
    }

  private def buildComposite(baseField: FormComponent, mf: MultiField, repeatable: Boolean): Option[Field] =
    mf.fields(baseField.id)
      .traverse { f =>
        formValuesByUnindexedId.get(f).map(v => (f, v))
      }
      .map(sequence(_))
      .map { v =>
        if (repeatable) buildRepeatableComposite(baseField.id, v)
        else buildNonRepeatableComposite(baseField.id, v.head)
      }

  private def buildRepeatableComposite(
    baseFieldId: FormComponentId,
    values: NonEmptyList[NonEmptyList[(FormComponentId, String)]]): Field =
    Field(
      FieldName(baseFieldId.value),
      ArrayNode(values.toList.map { buildObjectStructureForComposite(baseFieldId, _) }))

  private def buildNonRepeatableComposite(
    baseFieldId: FormComponentId,
    fields: NonEmptyList[(FormComponentId, String)]): Field =
    Field(FieldName(baseFieldId.value), buildObjectStructureForComposite(baseFieldId, fields))

  private def buildObjectStructureForComposite(
    baseFieldId: FormComponentId,
    fields: NonEmptyList[(FormComponentId, String)]) =
    ObjectStructure(fields.map {
      case (k, v) => Field(FieldName(k.stripBase(baseFieldId).value), TextNode(v))
    }.toList)

  private def buildSimpleField(
    field: FormComponent,
    values: NonEmptyList[String],
    repeatable: Boolean,
    multiValue: Boolean): Field =
    if (repeatable) buildRepeatingSimpleField(field, values, multiValue)
    else buildNonRepeatingSimpleField(field, values.head, multiValue)

  private def buildNonRepeatingSimpleField(field: FormComponent, value: String, multiValue: Boolean): Field =
    Field(FieldName(field.id.value), buildNode(value, multiValue))

  private def buildRepeatingSimpleField(field: FormComponent, value: NonEmptyList[String], multiValue: Boolean): Field =
    Field(FieldName(field.id.value), ArrayNode(value.toList.map(buildNode(_, multiValue))))

  private def buildNode(value: String, multiValue: Boolean): StructuredFormValue =
    if (multiValue) choicesToArray(value)
    else TextNode(value)

  private def extractMultiChoiceFieldIds(template: FormTemplate): Set[FormComponentId] = {
    def groupOrNot(field: FormComponent): List[FormComponent] = field.`type` match {
      case g: Group => g.fields
      case _        => List(field)
    }

    def multiChoiceFieldId(field: FormComponent): Option[FormComponentId] = field.`type` match {
      case c: Choice if c.`type` == Checkbox => field.id.some
      case _                                 => None
    }

    (for {
      section       <- template.sections
      field         <- section.fields
      nonGroupField <- groupOrNot(field)
      id            <- multiChoiceFieldId(nonGroupField)
    } yield id).toSet
  }

  private def choicesToArray(commaSeparatedString: String): ArrayNode =
    ArrayNode(commaSeparatedString.split(',').map(_.trim).filterNot(_.isEmpty).map(TextNode).toList)

  def sequence(
    map: NonEmptyList[(FormComponentId, NonEmptyList[String])]): NonEmptyList[NonEmptyList[(FormComponentId, String)]] =
    NonEmptyList.fromListUnsafe {
      (0 until map.head._2.size).toList.map { i =>
        map.map { case (k, v) => (k -> v.toList(i)) }
      }
    }
}
