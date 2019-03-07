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

package uk.gov.hmrc.gform.submission.handlebars

import cats.data.NonEmptyList
import cats.instances.option._
import cats.syntax.option._
import com.fasterxml.jackson.databind.JsonNode
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplate }
import uk.gov.hmrc.gform.formtemplate.RepeatingComponentService
import JsonNodes._

object StructuredFormDataBuilder {
  def apply(form: Form, template: FormTemplate): JsonNode =
    objectNode(new StructuredFormDataBuilder(form, template).build())
}

class StructuredFormDataBuilder(form: Form, template: FormTemplate) {
  private val formValuesByUnindexedId: Map[FormComponentId, NonEmptyList[String]] =
    form.formData.fields
      .groupBy(f => RepeatingComponentService.reduceToTemplateFieldId(f.id))
      .mapValues(values => NonEmptyList.fromListUnsafe(values.map(_.value).toList))

  private val multiChoiceFieldIds: Set[FormComponentId] = extractMultiChoiceFieldIds(template)

  def build(): Map[String, JsonNode] =
    (for {
      section     <- template.sections
      field       <- section.fields
      fieldAsJson <- buildField(field, RepeatingComponentService.isRepeatingSection(section))
    } yield fieldAsJson).map { case (k, v) => (k.id.value, v) }.toMap

  private def buildField(field: FormComponent, repeatable: Boolean): Seq[(FormComponent, JsonNode)] =
    field.`type` match {
      case g: Group => buildGroupFields(g)
      case _        => buildNonGroupField(field, repeatable).toSeq
    }

  private def buildGroupFields(group: Group): Seq[(FormComponent, JsonNode)] =
    group.fields.flatMap { buildNonGroupField(_, repeatable = true) }

  private def buildNonGroupField(nonGroupField: FormComponent, repeatable: Boolean): Option[(FormComponent, JsonNode)] =
    nonGroupField.`type` match {
      case mf: MultiField => buildComposite(nonGroupField, mf, repeatable)
      case _ =>
        formValuesByUnindexedId.get(nonGroupField.id).map {
          buildSimpleField(nonGroupField, _, repeatable, multiChoiceFieldIds.contains(nonGroupField.id))
        }
    }

  private def buildComposite(
    baseField: FormComponent,
    mf: MultiField,
    repeatable: Boolean): Option[(FormComponent, JsonNode)] =
    mf.fields(baseField.id)
      .traverse { f =>
        formValuesByUnindexedId.get(f).map(v => (f, v))
      }
      .map(sequence(_))
      .map { v =>
        (
          baseField,
          if (repeatable) buildRepeatableComposite(baseField.id, v)
          else buildNonRepeatableComposite(baseField.id, v.head))
      }

  private def buildRepeatableComposite(
    baseFieldId: FormComponentId,
    values: NonEmptyList[NonEmptyList[(FormComponentId, String)]]): JsonNode =
    arrayNode(values.toList.map { buildNonRepeatableComposite(baseFieldId, _) })

  private def buildNonRepeatableComposite(
    baseFieldId: FormComponentId,
    fields: NonEmptyList[(FormComponentId, String)]): JsonNode =
    objectNode(fields.map { case (k, v) => (k.stripBase(baseFieldId).value, textNode(v)) }.toList.toMap)

  private def buildSimpleField(
    field: FormComponent,
    values: NonEmptyList[String],
    repeatable: Boolean,
    multiValue: Boolean): (FormComponent, JsonNode) =
    if (repeatable) buildRepeatingSimpleField(field, values, multiValue)
    else buildNonRepeatingSimpleField(field, values.head, multiValue)

  private def buildNonRepeatingSimpleField(
    field: FormComponent,
    value: String,
    multiValue: Boolean): (FormComponent, JsonNode) =
    (field, buildNode(value, multiValue))

  private def buildRepeatingSimpleField(
    field: FormComponent,
    value: NonEmptyList[String],
    multiValue: Boolean): (FormComponent, JsonNode) =
    (field, arrayNode(value.toList.map(buildNode(_, multiValue))))

  private def buildNode(value: String, multiValue: Boolean): JsonNode =
    if (multiValue) choicesToArray(value)
    else textNode(value)

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

  private def choicesToArray(commaSeparatedString: String) =
    arrayNode(commaSeparatedString.split(',').map(_.trim).filterNot(_.isEmpty).map(textNode).toList)

  def sequence(
    map: NonEmptyList[(FormComponentId, NonEmptyList[String])]): NonEmptyList[NonEmptyList[(FormComponentId, String)]] =
    NonEmptyList.fromListUnsafe {
      (0 until map.head._2.size).toList.map { i =>
        map.map { case (k, v) => (k -> v.toList(i)) }
      }
    }
}
