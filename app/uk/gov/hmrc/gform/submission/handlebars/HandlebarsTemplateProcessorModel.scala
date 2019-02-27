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
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.node.{ ArrayNode, BaseJsonNode, ObjectNode }
import com.fasterxml.jackson.databind.node.JsonNodeFactory.{ instance => jsonNodeFactory }
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.databind.node.ArrayNode
import com.fasterxml.jackson.databind.node.JsonNodeFactory.{instance => jsonNodeFactory}
import uk.gov.hmrc.gform.formtemplate.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.{Form, Variables}
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{FormComponentId, FormTemplate}

import scala.collection.JavaConversions._

case class HandlebarsTemplateProcessorModel(model: JsonNode) extends AnyVal {
  def +(that: HandlebarsTemplateProcessorModel): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(fieldMap ++ that.fieldMap)
  private def fieldMap: Map[String, JsonNode] = model.fields.toList.map(e => e.getKey -> e.getValue).toMap
}

object HandlebarsTemplateProcessorModel {
  val empty: HandlebarsTemplateProcessorModel = HandlebarsTemplateProcessorModel(jsonNodeFactory.objectNode())

  def apply(jsonDocument: String): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(new ObjectMapper().readTree(jsonDocument))

  def apply(form: Form, template: FormTemplate): HandlebarsTemplateProcessorModel = {
    val groupedFields: Map[FormComponentId, NonEmptyList[String]] =
      form.formData.fields
        .groupBy(f => RepeatingComponentService.reduceToTemplateFieldId(f.id))
        .mapValues(values => NonEmptyList.fromListUnsafe(values.map(_.value).toList))

    val repeatableFieldIds: Set[FormComponentId] = RepeatingComponentService.extractRepeatableFieldIds(template)

    val formFields: Map[String, BaseJsonNode] =
      groupedFields
        .map {
          case (id, values) =>
            if (repeatableFieldIds(id))
              (id.value, new ArrayNode(jsonNodeFactory, values.toList.map(jsonNodeFactory.textNode)))
            else (id.value, jsonNodeFactory.textNode(values.head))
        }

    val rosmRegistration = {
      val f = form.thirdPartyData.desRegistrationResponse.fold("") _
      val regData: Map[String, BaseJsonNode] = Map(
        "safeId"           -> f(_.safeId),
        "organisationName" -> f(_.orgOrInd.getOrganisationName),
        "organisationType" -> f(_.orgOrInd.getOrganisationType),
        "isAGroup"         -> f(_.orgOrInd.getIsAGroup)
      ).mapValues(jsonNodeFactory.textNode)

      new ObjectNode(jsonNodeFactory, regData)
    }

    val extendedFields = formFields +
      ("formId"                    -> jsonNodeFactory.textNode(form._id.value)) +
      ("hmrcRosmRegistrationCheck" -> rosmRegistration)

    apply(extendedFields)
  }

  def apply(fields: Map[String, JsonNode]): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(jsonNodeFactory.objectNode().setAll(fields))

  def apply(variables: Variables): HandlebarsTemplateProcessorModel =
    apply(variables.value.toString)
}
