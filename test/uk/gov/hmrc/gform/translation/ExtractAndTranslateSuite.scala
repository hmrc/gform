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

class ExtractAndTranslateSuite extends FunSuite {

  List(
    (
      "Text with inline element <strong>element</strong>",
      List("Text with inline element <strong>element</strong>")
    ),
    (
      """|<div class="govuk-!-margin-top-7">
         |  <a href ="${link.printSummaryPdf}"> Save or print a draft copy of your answers (opens in new tab)</a>
         |  <h2 class="govuk-heading-m govuk-!-margin-top-8">Now submit your application</h2>
         |  I confirm that the information I have provided is true and complete.
         |</div>""",
      List(
        """<a href="${link.printSummaryPdf}"> Save or print a draft copy of your answers (opens in new tab)</a>""",
        "Now submit your application",
        "I confirm that the information I have provided is true and complete."
      )
    ),
    (
      """<p>If you are changing your business address.</p> <ul class="govuk-list"><li><a href="https://www.gov.uk/how-to">How to update details (opens in new tab)</a></li><li><a href="https://www.gov.uk/eori">What is an XI EORI number? (opens in new tab)</a></li></ul>""",
      List(
        "If you are changing your business address.",
        """<a href="https://www.gov.uk/how-to">How to update details (opens in new tab)</a>""",
        """<a href="https://www.gov.uk/eori">What is an XI EORI number? (opens in new tab)</a>"""
      )
    ),
    (
      """<table class="govuk-table govuk-!-margin-top-4">\n<caption class="govuk-table__caption govuk-table__caption--m">SME</caption>\n<thead class="govuk-table__head">\n<tr class="govuk-table__row">\n<th></th>\n<th></th>\n</tr>\n</thead>\n<tbody class="govuk-table__body">\n<tr class="govuk-table__row">\n<th scope="row" class="govuk-table__cell">Project qualifying expenditure so far</th>\n<td class="govuk-table__cell govuk-table__cell--numeric">${projectTotalNullSme}</td>\n</tr>\n<tr class="govuk-table__row">\n<th scope="row" class="govuk-table__header">Total project amount must account for at least</th>\n<td class="govuk-table__cell govuk-table__cell--numeric">${fiftyOnePerCentSme}</td>\n</tr>\n </tbody>\n</table>""",
      List(
        "SME",
        "Project qualifying expenditure so far",
        "Total project amount must account for at least"
      )
    ),
    (
      """<table class='govuk-table'><thead class='govuk-table__head'><tr class='govuk-table__row'><th scope='col' class='govuk-table__header'>Type of item</th><th scope='col' class='govuk-table__header'>Number of this type of item</th></tr></thead><tbody class='govuk-table__body'><tr class='govuk-table__row'><th scope='row' class='govuk-table__header'>TV</th><td class='govuk-table__cell'>2</td></tr><tr class='govuk-table__row'><th scope='row' class='govuk-table__header'>Tablet</th><td class='govuk-table__cell'>3</td></tr><tr class='govuk-table__row'><th scope='row' class='govuk-table__header'>Laptop</th><td class='govuk-table__cell'>1</td></tr></tbody><tbody class='govuk-table__body'><tr class='govuk-table__row'><th scope='row' class='govuk-table__header'>Clothing</th><td class='govuk-table__cell'>55</td></tr><tr class='govuk-table__row'><th scope='row' class='govuk-table__header'>Books</th><td class='govuk-table__cell'>110 (approximately)</td></tr><tr class='govuk-table__row'><th scope='row' class='govuk-table__header'>Cutlery</th><td class='govuk-table__cell'>40 (approximately)</td></tr><tr class='govuk-table__row'><th scope='row' class='govuk-table__header'>Crockery </th><td class='govuk-table__cell'>30 (approximately)</td></tr><tr class='govuk-table__row'><th scope='row' class='govuk-table__header'>Utensils </th><td class='govuk-table__cell'>25 (approximately)</td></tr></tbody></table> \n\n You do not need to include original costs of items, current values, or brands.""",
      List(
        "Type of item",
        "Number of this type of item",
        "TV",
        "Tablet",
        "Laptop",
        "Clothing",
        "Books",
        "110 (approximately)",
        "Cutlery",
        "40 (approximately)",
        "Crockery",
        "30 (approximately)",
        "Utensils",
        "25 (approximately)",
        """\n\n You do not need to include original costs of items, current values, or brands."""
      )
    ),
    (
      """EN1<p class="caption">EN2</p><p class="body">EN3</p><span>EN4</span>""",
      List("EN1", "EN2", "EN3", "<span>EN4</span>")
    ),
    (
      """|<p>To be eligible for R&D tax relief</p>
         |<p><span>contracted out R&D</span></p>""",
      List(
        "To be eligible for R&D tax relief",
        """<span>contracted out R&D</span>"""
      )
    ),
    (
      """|To be eligible for R&D tax relief there must be qualifying expenditure in one of these categories. Links all open in a new tab:
         |<ul>
         |  <li>
         |    <a href='https://cird84000'>externally provided workers</a>
         |  </li>
         |  <li>
         |    <a href='https://cird84200'>contracted out R&D</a>
         |  </li>
         |  <li>
         |    <a class='govuk-link' href='https://cird83000'>staffing</a>
         |  </li>
         |  <li>
         |    <a href='https://cird82500'>software</a>
         |  </li>
         |  <li>
         |    <a href='https://CIRD135000'>cloud computing</a>
         |  </li>
         |</ul>Next you'll need to tell us which categories incurred qualifying expenditure, and how much was spent. &nbsp; &nbsp;""",
      List(
        "To be eligible for R&D tax relief there must be qualifying expenditure in one of these categories. Links all open in a new tab:",
        """<a href="https://cird84000">externally provided workers</a>""",
        """<a href="https://cird84200">contracted out R&D</a>""",
        """<a class="govuk-link" href="https://cird83000">staffing</a>""",
        """<a href="https://cird82500">software</a>""",
        """<a href="https://CIRD135000">cloud computing</a>""",
        """Next you'll need to tell us which categories incurred qualifying expenditure, and how much was spent. &nbsp; &nbsp;"""
      )
    )
  ).zipWithIndex.foreach { case ((english, expectedBreakdown), index) =>
    test(s"html extraction ${index + 1}") {
      val translateTexts = ExtractAndTranslate(english.stripMargin).translateTexts.map(_.en)
      assertEquals(translateTexts, expectedBreakdown)
    }
  }

  List(
    (
      Map(
        (
          EnFromSpreadsheet("Pure text"),
          CyFromSpreadsheet("CY1")
        )
      ),
      "Pure text",
      "CY1"
    ),
    (
      Map(
        (
          EnFromSpreadsheet("<strong>Inline element</strong>"),
          CyFromSpreadsheet("<strong>CY1</strong>")
        )
      ),
      "<strong>Inline element</strong>",
      "<strong>CY1</strong>"
    ),
    (
      Map(
        (
          EnFromSpreadsheet("Text with inline element <strong>element</strong>"),
          CyFromSpreadsheet("CY1 <strong>CY2</strong>")
        )
      ),
      "Text with inline element <strong>element</strong>",
      "CY1 <strong>CY2</strong>"
    ),
    (
      Map(
        (
          EnFromSpreadsheet("Text with inline element <strong>element</strong>"),
          CyFromSpreadsheet("CY1 <strong>CY2</strong>")
        ),
        (
          EnFromSpreadsheet("Paragraph content"),
          CyFromSpreadsheet("CY3")
        ),
        (
          EnFromSpreadsheet("More text"),
          CyFromSpreadsheet("CY4")
        )
      ),
      """Text with inline element <strong>element</strong><p class="govuk-body">Paragraph content</p>More text""",
      """CY1 <strong>CY2</strong><p class="govuk-body">CY3</p>CY4"""
    ),
    (
      Map(
        (
          EnFromSpreadsheet("Type of item"),
          CyFromSpreadsheet("CY1")
        ),
        (
          EnFromSpreadsheet("Number of this type of item"),
          CyFromSpreadsheet("CY2")
        ),
        (
          EnFromSpreadsheet("TV"),
          CyFromSpreadsheet("CY3")
        ),
        (
          EnFromSpreadsheet("Tablet"),
          CyFromSpreadsheet("CY4")
        )
      ),
      """|<table class='table'>
         |  <thead class='table__head'>
         |    <tr class='table__row'>
         |      <th scope='col' class='table__header'>Type of item</th>
         |      <th scope='col' class='table__header'>Number of this type of item</th>
         |    </tr>
         |  </thead>
         |  <tbody class='table__body'>
         |    <tr class='table__row'>
         |      <th scope='row' class='table__header'>TV</th>
         |      <td class='table__cell'>2</td>
         |    </tr>
         |    <tr class='table__row'>
         |      <th scope='row' class='table__header'>Tablet</th>
         |      <td class='table__cell'>3</td>
         |    </tr>
         |  </tbody>
         |</table>""",
      """|<table class="table">
         |  <thead class="table__head">
         |    <tr class="table__row">
         |      <th scope="col" class="table__header">CY1</th>
         |      <th scope="col" class="table__header">CY2</th>
         |    </tr>
         |  </thead>
         |  <tbody class="table__body">
         |    <tr class="table__row">
         |      <th scope="row" class="table__header">CY3</th>
         |      <td class="table__cell">2</td>
         |    </tr>
         |    <tr class="table__row">
         |      <th scope="row" class="table__header">CY4</th>
         |      <td class="table__cell">3</td>
         |    </tr>
         |  </tbody>
         |</table>"""
    ),
    (
      Map(
        (
          EnFromSpreadsheet("EN1"),
          CyFromSpreadsheet("CY1")
        ),
        (
          EnFromSpreadsheet("EN2"),
          CyFromSpreadsheet("CY2")
        ),
        (
          EnFromSpreadsheet("EN3"),
          CyFromSpreadsheet("CY3")
        ),
        (
          EnFromSpreadsheet("<span>EN4</span>"),
          CyFromSpreadsheet("<span>CY4</span>")
        )
      ),
      """EN1<p class="caption">EN2</p><p class="body">EN3</p><span>EN4</span>""",
      """CY1<p class="caption">CY2</p><p class="body">CY3</p><span>CY4</span>"""
    ),
    (
      Map(
        (
          EnFromSpreadsheet("EN1"),
          CyFromSpreadsheet("CY1")
        ),
        (
          EnFromSpreadsheet("<a href='https://cird84000'>EN2</a>"),
          CyFromSpreadsheet("<a href='https://cird84000'>CY2</a>")
        ),
        (
          EnFromSpreadsheet("EN3"),
          CyFromSpreadsheet("CY3")
        ),
        (
          EnFromSpreadsheet("EN4"),
          CyFromSpreadsheet("CY4")
        )
      ),
      """EN1<ul><li><a href='https://cird84000'>EN2</a></li><li>EN3</li></ul>EN4""",
      """CY1<ul><li><a href="https://cird84000">CY2</a></li><li>CY3</li></ul>CY4"""
    ),
    (
      Map(
        (
          EnFromSpreadsheet(
            "To be eligible for R&D tax relief there must be qualifying expenditure in one of these categories. Links all open in a new tab:"
          ),
          CyFromSpreadsheet("CY1-A R&D CY1-B")
        ),
        (
          EnFromSpreadsheet("<a href='https://cird84000'>externally provided workers</a>"),
          CyFromSpreadsheet("<a href='https://cird84000'>CY2</a>")
        ),
        (
          EnFromSpreadsheet("<a href='https://cird84200'>contracted out R&D</a>"),
          CyFromSpreadsheet("<a href='https://cird84200'>CY3</a>")
        ),
        (
          EnFromSpreadsheet("<a class='govuk-link' href='https://cird83000'>staffing</a>"),
          CyFromSpreadsheet("<a class='govuk-link' href='https://cird83000'>CY4</a>")
        ),
        (
          EnFromSpreadsheet("<a href='https://cird82500'>software</a>"),
          CyFromSpreadsheet("<a href='https://cird82500'>CY5</a>")
        ),
        (
          EnFromSpreadsheet("<a href='https://CIRD135000'>cloud computing</a>"),
          CyFromSpreadsheet("<a href='https://CIRD135000'>CY6</a>")
        ),
        (
          EnFromSpreadsheet(
            "Next you'll need to tell us which categories incurred qualifying expenditure, and how much was spent. &nbsp; &nbsp;"
          ),
          CyFromSpreadsheet("CY7")
        )
      ),
      """|To be eligible for R&D tax relief there must be qualifying expenditure in one of these categories. Links all open in a new tab:
         |<ul>
         |  <li>
         |    <a href='https://cird84000'>externally provided workers</a>
         |  </li>
         |  <li>
         |    <a href='https://cird84200'>contracted out R&D</a>
         |  </li>
         |  <li>
         |    <a class='govuk-link' href='https://cird83000'>staffing</a>
         |  </li>
         |  <li>
         |    <a href='https://cird82500'>software</a>
         |  </li>
         |  <li>
         |    <a href='https://CIRD135000'>cloud computing</a>
         |  </li>
         |</ul>Next you'll need to tell us which categories incurred qualifying expenditure, and how much was spent. &nbsp; &nbsp;""",
      """|CY1-A R&D CY1-B
         |<ul>
         |  <li>
         |    <a href="https://cird84000">CY2</a>
         |  </li>
         |  <li>
         |    <a href="https://cird84200">CY3</a>
         |  </li>
         |  <li>
         |    <a class="govuk-link" href="https://cird83000">CY4</a>
         |  </li>
         |  <li>
         |    <a href="https://cird82500">CY5</a>
         |  </li>
         |  <li>
         |    <a href="https://CIRD135000">CY6</a>
         |  </li>
         |</ul>CY7"""
    ),
    (
      (
        Map.empty[EnFromSpreadsheet, CyFromSpreadsheet],
        """<p>${abc}</p>""",
        """<p>${abc}</p>"""
      ),
    ),
    (
      (
        Map(
          (
            EnFromSpreadsheet("Hello"),
            CyFromSpreadsheet("CY1")
          )
        ),
        """<div><p>Hello</p><p>1234</p></div>""",
        """<div><p>CY1</p><p>1234</p></div>"""
      ),
    )
  ).zipWithIndex.foreach { case ((spreadsheetData, english, expectedWelsh), index) =>
    val spreadsheet = Spreadsheet(spreadsheetData)
    test(s"html translation ${index + 1}") {
      val translateTexts = ExtractAndTranslate(english.stripMargin).translate(spreadsheet)
      assertEquals(translateTexts, expectedWelsh.stripMargin)
    }
  }
}
