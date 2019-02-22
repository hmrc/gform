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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations

import uk.gov.hmrc.gform.Spec

class SingleQuoteReplacementLexerSpec extends Spec {
  "Non-string literals" should "be copied verbatim" in {
    SingleQuoteReplacementLexer("abc") shouldBe Right("abc")
  }

  "Single quoted strings" should "be replaced by double quoted" in {
    SingleQuoteReplacementLexer("'abc'") shouldBe Right(""""abc"""")
  }

  it should "replace double quotes with escaped double quotes" in {
    SingleQuoteReplacementLexer("""'ab"c'""") shouldBe Right(""""ab\"c"""")
  }

  it should "replace escaped single quotes with unescaped single quotes" in {
    SingleQuoteReplacementLexer("""'ab\'c'""") shouldBe Right(""""ab'c"""")
  }

  it should "copy escaped unicode characters verbatim" in {
    SingleQuoteReplacementLexer("""'\u1234'""") shouldBe Right(""""\u1234"""")
  }

  it should "copy escaped non-unicode characters verbatim" in {
    SingleQuoteReplacementLexer("""'\t\r\n\b\f'""") shouldBe Right(""""\t\r\n\b\f"""")
  }

  "Mixtures of non-string literals and single quoted strings" should "be processed" in {
    SingleQuoteReplacementLexer("""'abc' 123 'def' 456""") shouldBe Right(""""abc" 123 "def" 456""")
  }

  "Power quotes" should "be replaced with quotes and the content should be copied verbatim" in {
    SingleQuoteReplacementLexer("abc ^def 'aa'^ ^0^") shouldBe Right("""abc "def 'aa'" "0"""")
  }
}
