/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.translation

import munit.FunSuite

class ExprMaskerSuite extends FunSuite {

  test("ExprMasker - mask/unmask (1)") {
    val input = "foo ${1 + 1} bar ${2 + 2} baz ${1 + 1}"
    val (result, lookup) = ExprMasker.mask(input)
    val expected = """foo ${redacted1} bar ${redacted2} baz ${redacted1}"""

    val expectedLookup = Map(
      (0, "${1 + 1}"),
      (1, "${2 + 2}")
    )

    assertEquals(result, expected)
    assertEquals(lookup, expectedLookup)

    val roudtrip = ExprMasker.unmask(expected, expectedLookup)
    assertEquals(input, roudtrip)
  }

  test("ExprMasker - mask/unmask (2)") {
    val input = "foo ${1 + 1} bar ${2 + 2} baz ${1 + 1 }"
    val (result, lookup) = ExprMasker.mask(input)
    val expected = """foo ${redacted1} bar ${redacted2} baz ${redacted3}"""

    val expectedLookup = Map(
      (0, "${1 + 1}"),
      (1, "${2 + 2}"),
      (2, "${1 + 1 }")
    )

    assertEquals(result, expected)
    assertEquals(lookup, expectedLookup)

    val roudtrip = ExprMasker.unmask(expected, expectedLookup)
    assertEquals(input, roudtrip)

  }

}
