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
import cats.syntax.option._
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.node.{ ArrayNode, BaseJsonNode, ObjectNode }
import com.fasterxml.jackson.databind.node.JsonNodeFactory.{ instance => jsonNodeFactory }
import uk.gov.hmrc.gform.formtemplate.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.collection.JavaConversions._

case class HandlebarsTemplateProcessorModel(model: JsonNode) extends AnyVal {
  def +(that: HandlebarsTemplateProcessorModel): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(fieldMap ++ that.fieldMap)
  private def fieldMap: Map[String, JsonNode] = model.fields.toList.map(e => e.getKey -> e.getValue).toMap
}

object HandlebarsTemplateProcessorModel {
  val empty: HandlebarsTemplateProcessorModel = HandlebarsTemplateProcessorModel(objectNode(Map.empty))

  def apply(jsonDocument: String): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(new ObjectMapper().readTree(jsonDocument))

  def apply(form: Form, template: FormTemplate): HandlebarsTemplateProcessorModel = {
    val groupedFields: Map[FormComponentId, NonEmptyList[String]] =
      form.formData.fields
        .groupBy(f => RepeatingComponentService.reduceToTemplateFieldId(f.id))
        .mapValues(values => NonEmptyList.fromListUnsafe(values.map(_.value).toList))

    val repeatableFieldIds: Set[FormComponentId] = RepeatingComponentService.extractRepeatableFieldIds(template)

    val multiChoiceFieldIds: Set[FormComponentId] = extractMultiChoiceFieldIds(template)

    val formFields: Map[String, BaseJsonNode] =
      groupedFields
        .map {
          case (id, values) =>
            (repeatableFieldIds(id), multiChoiceFieldIds(id)) match {
              case (false, false) => (id.value, textNode(values.head))
              case (false, true)  => (id.value, choicesToArray(values.head))
              case (true, false)  => (id.value, arrayNode(values.toList.map(textNode)))
              case (true, true)   => (id.value, arrayNode(values.toList.map(choicesToArray)))
            }
        }

    val rosmRegistration = {
      val f = form.thirdPartyData.desRegistrationResponse.fold("") _
      val regData: Map[String, BaseJsonNode] = Map(
        "safeId"           -> f(_.safeId),
        "organisationName" -> f(_.orgOrInd.getOrganisationName),
        "organisationType" -> f(_.orgOrInd.getOrganisationType),
        "isAGroup"         -> f(_.orgOrInd.getIsAGroup)
      ).mapValues(textNode)

      objectNode(regData)
    }

    val extendedFields = formFields +
      ("formId"                    -> textNode(form._id.value)) +
      ("hmrcRosmRegistrationCheck" -> rosmRegistration)

    apply(extendedFields)
  }

  def apply(fields: Map[String, JsonNode]): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(objectNode(fields))

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
    arrayNode(commaSeparatedString.split(',').map(_.trim).map(textNode).toList)

  private def textNode(s: String) = jsonNodeFactory.textNode(s)
  private def objectNode(fields: Map[String, JsonNode]) = new ObjectNode(jsonNodeFactory, fields)
  private def arrayNode(elements: List[JsonNode]) = new ArrayNode(jsonNodeFactory, elements)
}
