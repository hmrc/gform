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

import com.github.jknack.handlebars.{ Context, EscapingStrategy, Handlebars, JsonNodeValueResolver }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.HandlebarsTemplateProcessorModel

trait HandlebarsTemplateProcessor {
  def apply(template: String, model: HandlebarsTemplateProcessorModel): String
}

class RealHandlebarsTemplateProcessor(
  helpers: HandlebarsTemplateProcessorHelpers = new HandlebarsTemplateProcessorHelpers())
    extends HandlebarsTemplateProcessor {

  object JsonEscapingStrategy extends EscapingStrategy {
    override def escape(value: CharSequence): CharSequence =
      if (value == null) null
      else org.apache.commons.lang3.StringEscapeUtils.unescapeJson(value.toString)
  }
  private val handlebars = new Handlebars().`with`(JsonEscapingStrategy).registerHelpers(helpers)
  def apply(template: String, model: HandlebarsTemplateProcessorModel): String = {
    val compiledTemplate = handlebars.compileInline(template)

    val context = Context
      .newBuilder(model.model)
      .resolver(JsonNodeValueResolver.INSTANCE)
      .build

    try {
      MagicCommasParser(compiledTemplate.apply(context))
    } catch {
      case ex: Exception => throw new Exception(model.model.toString, ex)
    }
  }
}
