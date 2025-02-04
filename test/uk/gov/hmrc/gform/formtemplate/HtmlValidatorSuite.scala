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

package uk.gov.hmrc.gform.formtemplate

import munit.FunSuite

class HtmlValidatorSuite extends FunSuite {

  val validHtmls = List(
    // These test strings cover usage patterns from real forms.
    "<ol><li style='${if countTRfilms = 0 then 'display:none' else ''}'></li></ol>",
    "${foo}<br/> ${bar}",
    "<span>&nbsp;</span>",
    "Department for Environment, Food & Rural Affairs (Defra)",
    "Start now &gt;",
    "<p>${businessName} does not meet the intensity criteria for this accounting period.\n\n You might still be eligible to claim for R&D expenditure credit (RDEC).</p><p>The new RDEC scheme is open to any company doing qualifying R&D. The expenditure credit is 20% of your qualifying R&D expenditure. <a href='https://www.gov.uk/guidance/corporation-tax-research-and-development-tax-relief-for-large-companies#rd-expenditure-credit-rdec'>About RDEC (opens in new tab)</a></p>To save the answers you’ve given in this form <a href='${link.printSummaryPdf}' download='${link.printSummaryPdf}' >download your answers as a PDF</a>"
  ).zipWithIndex.foreach { case (validHtml, index) =>
    test(s"valid html (${index + 1})") {
      val res = HtmlValidator.validate(List(validHtml -> ""))
      assertEquals(res, None)
    }
  }

  List(
    (
      "${businessName} to claim for R&D expenditure credit (RDEC).</p>",
      "${redacted} to claim for R&D expenditure credit (RDEC).<p></p>"
    ),
    ("Start now >", "Start now &gt;"),
    ("<b>Payment reference&nbsp</b>", "<b>Payment reference&nbsp;</b>"),
    (
      """<h2 clAss="govuk-heading-l">How to pay</h2>""",
      """<h2 class='govuk-heading-l'>How to pay</h2>"""
    )
  ).zipWithIndex.foreach { case ((invalidHtml, expectedFix), index) =>
    test(s"detect invalid html and provide hint for the fix (${index + 1})") {
      val res = HtmlValidator.validate(List(invalidHtml -> ""))
      res match {
        case None => fail(s"This html was wrongly determined to be valid: $invalidHtml")
        case Some(htmlValidatorResult) =>
          assertEquals(htmlValidatorResult.possibleFix, expectedFix)
      }
    }
  }
}
