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
import uk.gov.hmrc.gform.sharedmodel.{ PdfHtml, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ HandlebarsTemplateProcessorModel, SingleQuoteReplacementLexer, TemplateType }
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue

class HandlebarsTemplateProcessorSpec extends Spec {
  "apply" must "work with JSON" in {
    process("I am a {{name}} template", """{"name" : "handlebars"}""", TemplateType.JSON) shouldBe "I am a handlebars template"
  }

  it must "work with XML" in {
    process("<foo>I am a {{name}} template</foo>", """{"name" : "handlebars"}""", TemplateType.XML) shouldBe "<foo>I am a handlebars template</foo>"
  }

  it must "work with more complex XML" in {
    SingleQuoteReplacementLexer(
      "<foo>I am a {{match ^('0') => '<AdoptionCounselling>Yes</AdoptionCounselling>'; ('1') => '<AdoptionAdvice>Yes</AdoptionAdvice>';^ name}} template</foo>") match {
      case Left(err) => fail(err)
      case Right(template) =>
        process(
          template,
          """{"name" : "0"}""",
          TemplateType.XML
        ) shouldBe "<foo>I am a <AdoptionCounselling>Yes</AdoptionCounselling> template</foo>"
    }
  }

  it must "work with plain text" in {
    process("I am a {{name}} template", """{"name" : "handlebars"}""", TemplateType.XML) shouldBe "I am a handlebars template"
  }

  "JsonEscapingStrategy un-escaping" must "work with apostrophes" in {
    process("string with an {{apostrophe}}", """{"apostrophe" : "'"}""", TemplateType.JSON) shouldBe "string with an '"
  }

  it must "work with a backslash" in {
    process("""string with a \""", TemplateType.JSON) shouldBe """string with a \"""
  }

  it must "work with two backslashes" in {
    process("""string with two \\""", TemplateType.JSON) shouldBe """string with two \\"""
  }

  private def process(functionCall: String, stringModel: String, templateType: TemplateType): String = {
    val model = HandlebarsTemplateProcessorModel(stringModel.stripMargin)
    process(
      functionCall,
      FocussedHandlebarsModelTree(
        HandlebarsModelTree(SubmissionRef(""), null, PdfHtml(""), StructuredFormValue.ObjectStructure(Nil), model)),
      templateType
    )
  }

  private def process(functionCall: String, templateType: TemplateType): String =
    process(functionCall, HandlebarsTemplateProcessorModel.empty, templateType)

  private def process(
    functionCall: String,
    model: HandlebarsTemplateProcessorModel,
    templateType: TemplateType): String =
    process(
      functionCall,
      FocussedHandlebarsModelTree(
        HandlebarsModelTree(SubmissionRef(""), null, PdfHtml(""), StructuredFormValue.ObjectStructure(Nil), model)),
      templateType
    )

  private def process(functionCall: String, tree: FocussedHandlebarsModelTree, templateType: TemplateType): String =
    RealHandlebarsTemplateProcessor(functionCall, HandlebarsTemplateProcessorModel.empty, tree, templateType)
}
