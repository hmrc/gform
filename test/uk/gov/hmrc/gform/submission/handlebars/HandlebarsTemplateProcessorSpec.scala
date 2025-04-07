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

package uk.gov.hmrc.gform.submission.handlebars

import com.fasterxml.jackson.databind.JsonNode
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.JsonNodes
import uk.gov.hmrc.gform.sharedmodel.{ PdfContent, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ HandlebarsTemplateProcessorModel, SingleQuoteReplacementLexer, TemplateType }
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue

class HandlebarsTemplateProcessorSpec extends Spec {
  "apply" must "work with JSON" in {
    processJson(
      "I am a {{name}} template",
      """{"name" : "handlebars"}""",
      TemplateType.JSON
    ) shouldBe "I am a handlebars template"
  }

  it must "work with XML" in {
    processJson(
      "<foo>I am a {{name}} template</foo>",
      """{"name" : "handlebars"}""",
      TemplateType.XML
    ) shouldBe "<foo>I am a handlebars template</foo>"
  }

  it must "work with more complex XML" in {
    SingleQuoteReplacementLexer(
      "<foo>I am a {{match ^('0') => '<AdoptionCounselling>Yes</AdoptionCounselling>'; ('1') => '<AdoptionAdvice>Yes</AdoptionAdvice>';^ name}} template</foo>"
    ) match {
      case Left(err) => fail(err)
      case Right(template) =>
        processJson(
          template,
          """{"name" : "0"}""",
          TemplateType.XML
        ) shouldBe "<foo>I am a <AdoptionCounselling>Yes</AdoptionCounselling> template</foo>"
    }
  }

  it must "work with plain text" in {
    processJson(
      "I am a {{name}} template",
      """{"name" : "handlebars"}""",
      TemplateType.XML
    ) shouldBe "I am a handlebars template"
  }

  "JsonEscapingStrategy escaping" must "ignore apostrophes" in {
    processModel(
      "string with an ' {{apostrophe}}",
      Map(("apostrophe", JsonNodes.textNode("'"))),
      TemplateType.JSON
    ) shouldBe "string with an ' '"
  }

  it must "escape a backslash" in {
    processModel(
      """string with a \ {{dataBacklash}}""",
      Map(("dataBacklash", JsonNodes.textNode("""\"""))),
      TemplateType.JSON
    ) shouldBe """string with a \ \\"""
  }

  it must "escape two backslashes" in {
    processModel(
      """string with two \\ {{dataBacklash}}""",
      Map(("dataBacklash", JsonNodes.textNode("""\\"""))),
      TemplateType.JSON
    ) shouldBe """string with two \\ \\\\"""
  }

  it must "escape double quotes" in {
    processModel(
      """string with {{foo}} two""",
      Map(("foo", JsonNodes.textNode("""b"a"r"""))),
      TemplateType.JSON
    ) shouldBe """string with b\"a\"r two"""
  }

  it must "escape control characters" in {
    processModel(
      """string with {{foo}} two""",
      Map(("foo", JsonNodes.textNode("""b\t"""))),
      TemplateType.JSON
    ) shouldBe """string with b\\t two"""
  }

  private def processJson(functionCall: String, jsonModel: String, templateType: TemplateType): String = {
    val model = HandlebarsTemplateProcessorModel(jsonModel)
    process(
      functionCall,
      FocussedHandlebarsModelTree(
        HandlebarsModelTree(
          FormId("someFormId"),
          SubmissionRef(""),
          null,
          PdfContent(""),
          None,
          StructuredFormValue.ObjectStructure(Nil),
          model,
          EnvelopeId("some-envelope-id")
        )
      ),
      templateType
    )
  }

  private def processModel(
    functionCall: String,
    model: Map[String, JsonNode],
    templateType: TemplateType
  ): String =
    process(
      functionCall,
      FocussedHandlebarsModelTree(
        HandlebarsModelTree(
          FormId("someFormId"),
          SubmissionRef(""),
          null,
          PdfContent(""),
          None,
          StructuredFormValue.ObjectStructure(Nil),
          HandlebarsTemplateProcessorModel(model),
          EnvelopeId("some-envelope-id")
        )
      ),
      templateType
    )

  private def process(functionCall: String, tree: FocussedHandlebarsModelTree, templateType: TemplateType): String =
    RealHandlebarsTemplateProcessor(functionCall, HandlebarsTemplateProcessorModel.empty, tree, templateType)
}
