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

package uk.gov.hmrc.gform.submission.handlebars

import cats.Endo
import com.github.jknack.handlebars.{ Context, EscapingStrategy, Handlebars, JsonNodeValueResolver }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ HandlebarsTemplateProcessorModel, TemplateType }

trait HandlebarsTemplateProcessor {
  def apply(
    template: String,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    focussedTree: FocussedHandlebarsModelTree,
    templateType: TemplateType): String
}

object RealHandlebarsTemplateProcessor extends HandlebarsTemplateProcessor {
  private class Processor(escapingStrategy: EscapingStrategy, postProcessor: Endo[String])
      extends RecursiveHandlebarsTemplateProcessor {

    def apply(
      template: String,
      accumulatedModel: HandlebarsTemplateProcessorModel,
      focussedTree: FocussedHandlebarsModelTree): String = {
      val helpers: HandlebarsTemplateProcessorHelpers =
        new HandlebarsTemplateProcessorHelpers(accumulatedModel, focussedTree.tree, this)
      val handlebars = new Handlebars().`with`(escapingStrategy).registerHelpers(helpers)

      val compiledTemplate = handlebars.compileInline(template)

      val context = Context
        .newBuilder((focussedTree.focus + accumulatedModel).model)
        .resolver(JsonNodeValueResolver.INSTANCE)
        .build

      try {
        postProcessor(compiledTemplate.apply(context))
      } catch {
        case ex: Exception => throw new Exception(focussedTree.focus.model.toString, ex)
      }
    }
  }

  private val jsonProcessor: Processor = new Processor(
    new EscapingStrategy {
      override def escape(value: CharSequence): CharSequence =
        if (value == null) null
        else org.apache.commons.text.StringEscapeUtils.unescapeJson(value.toString)
    },
    MagicCommasParser.apply
  )

  private val xmlProcessor: Processor = new Processor(
    EscapingStrategy.XML,
    identity
  )

  private val plainProcessor: Processor = new Processor(
    EscapingStrategy.NOOP,
    identity
  )

  override def apply(
    template: String,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    focussedTree: FocussedHandlebarsModelTree,
    templateType: TemplateType): String =
    (templateType match {
      case TemplateType.JSON  => jsonProcessor
      case TemplateType.XML   => xmlProcessor
      case TemplateType.Plain => plainProcessor
    })(template, accumulatedModel, focussedTree)
}
