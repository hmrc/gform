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

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ HandlebarsTemplateProcessorModel, SingleQuoteReplacementLexer, TemplateType }

class HandlebarsTemplateProcessorSpec extends Spec {
  "apply" must "work with JSON" in {
    RealHandlebarsTemplateProcessor(
      "I am a {{name}} template",
      HandlebarsTemplateProcessorModel("""{"name" : "handlebars"}"""),
      TemplateType.JSON) shouldBe "I am a handlebars template"
  }

  it must "work with XML" in {
    RealHandlebarsTemplateProcessor(
      "<foo>I am a {{name}} template</foo>",
      HandlebarsTemplateProcessorModel("""{"name" : "handlebars"}"""),
      TemplateType.XML) shouldBe "<foo>I am a handlebars template</foo>"
  }

  it must "work with more complex XML" in {
    SingleQuoteReplacementLexer(
      "<foo>I am a {{match ^('0') => '<AdoptionCounselling>Yes</AdoptionCounselling>'; ('1') => '<AdoptionAdvice>Yes</AdoptionAdvice>';^ name}} template</foo>") match {
      case Left(err) => fail(err)
      case Right(template) =>
        RealHandlebarsTemplateProcessor(
          template,
          HandlebarsTemplateProcessorModel("""{"name" : "0"}"""),
          TemplateType.XML
        ) shouldBe "<foo>I am a <AdoptionCounselling>Yes</AdoptionCounselling> template</foo>"
    }
  }

  it must "work with plain text" in {
    RealHandlebarsTemplateProcessor(
      "I am a {{name}} template",
      HandlebarsTemplateProcessorModel("""{"name" : "handlebars"}"""),
      TemplateType.XML) shouldBe "I am a handlebars template"
  }

  "JsonEscapingStrategy un-escaping" must "work with apostrophes" in {
    RealHandlebarsTemplateProcessor(
      "string with an {{apostrophe}}",
      HandlebarsTemplateProcessorModel("""{"apostrophe" : "'"}"""),
      TemplateType.JSON) shouldBe "string with an '"
  }

  it must "work with a backslash" in {
    RealHandlebarsTemplateProcessor("""string with a \""", HandlebarsTemplateProcessorModel.empty, TemplateType.JSON) shouldBe """string with a \"""
  }

  it must "work with two backslashes" in {
    RealHandlebarsTemplateProcessor("""string with two \\""", HandlebarsTemplateProcessorModel.empty, TemplateType.JSON) shouldBe """string with two \\"""
  }
}
