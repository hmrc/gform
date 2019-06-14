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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.HandlebarsTemplateProcessorModel

class HandlebarsTemplateProcessorSpec extends Spec {
  "apply" must "work" in {
    val processor = new RealHandlebarsTemplateProcessor
    processor("I am a {{name}} template", HandlebarsTemplateProcessorModel("""{"name" : "handlebars"}""")) shouldBe "I am a handlebars template"
  }

  "JsonEscapingStrategy un-escaping" must "work with apostrophes" in {
    val a = new RealHandlebarsTemplateProcessor
    a.JsonEscapingStrategy.escape("string with an ' apostrophe") shouldBe "string with an ' apostrophe"
  }

  "it" must "work with a backslash" in {
    val a = new RealHandlebarsTemplateProcessor
    a.JsonEscapingStrategy.escape("""string with a \ backslash""") shouldBe """string with a  backslash"""
  }

  "it" must "work with two backslashes" in {
    val a = new RealHandlebarsTemplateProcessor
    a.JsonEscapingStrategy.escape("""string with \\ backslashes""") shouldBe """string with \ backslashes"""
  }

  "it" must "work with multiple backslashes" in {
    val a = new RealHandlebarsTemplateProcessor
    a.JsonEscapingStrategy.escape("""string with \\\' backslashes""") shouldBe """string with \' backslashes"""
  }
}
