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

import uk.gov.hmrc.gform.Spec

class LookupMatchStringParserSpec extends Spec {
  "apply" must "return a single matching case for a zero length key" in {
    LookupMatchStringParser("() => 'A'", List()) shouldBe Option("A")
  }

  it must "return None if no cases match" in {
    LookupMatchStringParser("() => 'A'", List("a")) shouldBe None
  }

  it must "return a single matching case for a simple key" in {
    LookupMatchStringParser("('0') => 'A'", List("0")) shouldBe Option("A")
  }

  it must "return a None for a non-matching simple key" in {
    LookupMatchStringParser("('0') => 'A'", List("1")) shouldBe None
  }

  it must "return a single matching case for a composite key" in {
    LookupMatchStringParser("('0' '1') => 'A'", List("0", "1")) shouldBe Option("A")
  }

  it must "return a None for a non-matching composite key" in {
    LookupMatchStringParser("('0' '1') => 'A'", List("0", "2")) shouldBe None
  }

  it must "return the first matching case for a composite key" in {
    LookupMatchStringParser("('0' '0') => 'A'; ('0' '1') => 'B'; ('1' '1') => 'C'", List("0", "1")) shouldBe Option("B")
  }

  it must "return the first matching case for a simple wildcarded key" in {
    LookupMatchStringParser("('0') => 'A'; (*) => 'B'; ('1') => 'C'", List("1")) shouldBe Option("B")
  }

  it must "return the first matching case for a composite key with a wildcard" in {
    LookupMatchStringParser("('0' '0') => 'A'; (* '1') => 'B'; ('1' '1') => 'C'", List("1", "1")) shouldBe Option("B")
  }
}
