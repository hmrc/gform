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
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.node.JsonNodeFactory.{ instance => jsonNodeFactory }
import uk.gov.hmrc.gform.sharedmodel.form.Form

import scala.collection.JavaConversions._

case class HandlebarsTemplateProcessorModel(model: JsonNode) extends AnyVal

object HandlebarsTemplateProcessorModel {
  def apply(jsonDocument: String): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(new ObjectMapper().readTree(jsonDocument))

  def apply(form: Form): HandlebarsTemplateProcessorModel = {
    val scalaFields = form.formData.fields
      .map { f =>
        (f.id.value, jsonNodeFactory.textNode(f.value))
      }
      .toMap[String, JsonNode]

    apply(jsonNodeFactory.objectNode().setAll(scalaFields))
  }
}
