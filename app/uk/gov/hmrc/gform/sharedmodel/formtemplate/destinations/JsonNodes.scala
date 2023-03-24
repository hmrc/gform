/*
 * Copyright 2023 HM Revenue & Customs
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

import com.fasterxml.jackson.databind.node.{ ArrayNode, ObjectNode }
import com.fasterxml.jackson.databind.{ JsonNode, ObjectMapper }
import com.fasterxml.jackson.databind.node.JsonNodeFactory.{ instance => jnf }

import scala.jdk.CollectionConverters._

object JsonNodes {
  def parseJson(json: String): JsonNode = new ObjectMapper().readTree(json)

  def textNode(s: String): JsonNode = jnf.textNode(s)
  def objectNode(fields: Map[String, JsonNode]): JsonNode = new ObjectNode(jnf, fields.asJava)
  def objectNode(fields: (String, JsonNode)*): JsonNode = new ObjectNode(jnf, fields.toMap[String, JsonNode].asJava)
  def arrayNode(elements: List[JsonNode]): JsonNode = new ArrayNode(jnf, elements.asJava)
  def numberNode(n: Int): JsonNode = jnf.numberNode(n)
}
