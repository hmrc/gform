/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations

import com.fasterxml.jackson.databind.JsonNode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.JsonNodes._
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue.{ ArrayNode, ObjectStructure, TextNode }
import uk.gov.hmrc.gform.sharedmodel.structuredform.{ Field, StructuredFormValue }

object JsonStructuredFormDataBuilder {
  def apply(structuredFormData: StructuredFormValue.ObjectStructure): JsonNode =
    objectNode(objectStructureToFieldList(structuredFormData))

  private def arrayElementsToJsonNodeList(elements: List[StructuredFormValue]): List[JsonNode] =
    elements.map(structuredFormValueToJsonNode)

  private def structuredFormValueToJsonNode(value: StructuredFormValue): JsonNode = value match {
    case objectStructure: ObjectStructure => objectNode(objectStructureToFieldList(objectStructure))
    case array: ArrayNode                 => arrayNode(arrayElementsToJsonNodeList(array.elements))
    case text: TextNode                   => textNode(text.value)
  }

  private def buildFieldList(fields: List[Field]): Map[String, JsonNode] =
    fields.map { case Field(name, value, _) => name.name -> structuredFormValueToJsonNode(value) }.toMap

  private def objectStructureToFieldList(structure: ObjectStructure): Map[String, JsonNode] =
    buildFieldList(structure.fields)

}
