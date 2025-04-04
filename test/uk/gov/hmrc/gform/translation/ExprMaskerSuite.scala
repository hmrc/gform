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
    val expected = "foo ${¦redacted0¦} bar ${¦redacted1¦} baz ${¦redacted0¦}"

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
    val expected = "foo ${¦redacted0¦} bar ${¦redacted1¦} baz ${¦redacted2¦}"

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

  test("ExprMasker - mask/unmask (3)") {
    val input =
      "<strong>Accounting period $n</strong>   <br>${accountingPeriod}<br>Amount: ${taxAmountOwedCalculated orElse amountOfferedVR} <br> <span class='${if isVoluntaryRestitution then 'govuk-tag'  else '' }'>${if isVoluntaryRestitution then 'Voluntary payment'  else '' }</span>"
    val (result, lookup) = ExprMasker.mask(input)
    val expected =
      "<strong>Accounting period $n</strong>   <br>${¦redacted0¦}<br>Amount: ${¦redacted1¦} <br> <span class='${¦redacted2¦}'>${¦redacted3¦}</span>"

    val expectedLookup = Map(
      (0, "${accountingPeriod}"),
      (1, "${taxAmountOwedCalculated orElse amountOfferedVR}"),
      (2, "${if isVoluntaryRestitution then 'govuk-tag'  else '' }"),
      (3, "${if isVoluntaryRestitution then 'Voluntary payment'  else '' }")
    )

    assertEquals(result, expected)
    assertEquals(lookup, expectedLookup)

    val roudtrip = ExprMasker.unmask(expected, expectedLookup)
    assertEquals(input, roudtrip)

  }

  test("ExprMasker - mask/unmask (4)") {
    val input =
      "## What happens next\n\n ${if consentToEmailChoice contains 1 then 'We will now consider your application.\n\n We aim to get in touch within 30 days.' else concat('We will now consider your application.\n\n We aim to get in touch within 60 days.', contactEmail, '')}"

    val (result, lookup) = ExprMasker.mask(input)
    val expected = "## What happens next\n\n ${¦redacted0¦}"

    val expectedLookup = Map(
      (
        0,
        "${if consentToEmailChoice contains 1 then 'We will now consider your application.\n\n We aim to get in touch within 30 days.' else concat('We will now consider your application.\n\n We aim to get in touch within 60 days.', contactEmail, '')}"
      )
    )

    assertEquals(result, expected)
    assertEquals(lookup, expectedLookup)

    val roudtrip = ExprMasker.unmask(expected, expectedLookup)
    assertEquals(input, roudtrip)

  }

}
