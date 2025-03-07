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
        """You do not need to include original costs of items, current values, or brands."""
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
    ),
    (
      """<h3 class='govuk-heading-l'>Amount to pay now: ${grandTotal}</h3>\n\nYou need to use the banking details below. Save this information for your records. It cannot be emailed to you for security reasons.""",
      List(
        "Amount to pay now: ${grandTotal}",
        """You need to use the banking details below. Save this information for your records. It cannot be emailed to you for security reasons."""
      )
    ),
    (
      """You need to send us all of the relevant forms within **24 hours**.\n\nIf you do not send us these details in time, you will need to start a new application.\n\nWhen you are ready to submit all of your forms, you will need to [return to each of your saved forms](/submissions/new-form/AEO-journey-selector) to submit them.\n\nIf you have sent us all the relevant details, we will tell you within 90 calendar days if your application has been successful.\n\nIf your application is unsuccessful we will tell you why.""",
      List(
        "You need to send us all of the relevant forms within **24 hours**.",
        "If you do not send us these details in time, you will need to start a new application.",
        "When you are ready to submit all of your forms, you will need to [return to each of your saved forms](/submissions/new-form/AEO-journey-selector) to submit them.",
        "If you have sent us all the relevant details, we will tell you within 90 calendar days if your application has been successful.",
        "If your application is unsuccessful we will tell you why."
      )
    ),
    (
      """On the next pages we will ask you some details, including: <ul><li>where you live</li><li>if you're eligible for this allowance</li><li>when you were registered as blind (for England and Wales)</li><li>the local authority you are registered blind with (for England and Wales)</li><li>the allowance amount</li></ul> \n\n<div class=\"govuk-inset-text\">\nYou can only tell us about one tax year at a time.\n</div>""",
      List(
        "On the next pages we will ask you some details, including:",
        "where you live",
        "if you're eligible for this allowance",
        "when you were registered as blind (for England and Wales)",
        "the local authority you are registered blind with (for England and Wales)",
        "the allowance amount",
        "You can only tell us about one tax year at a time."
      )
    ),
    (
      """##Blind Person’s allowance\n\n\nYou can tell us if: <ul><li>the allowance is missing from your Personal tax account </li><li>you no longer receive this allowance</li></ul>\n\n""",
      List(
        "Blind Person’s allowance",
        "You can tell us if:",
        "the allowance is missing from your Personal tax account",
        "you no longer receive this allowance"
      )
    ),
    (
      """###What has changed \n \n You can now operate PAYE on a proportion of the employee’s income.""",
      List(
        "What has changed",
        "You can now operate PAYE on a proportion of the employee’s income."
      )
    ),
    (
      """For each animated television or film, we’ll ask for:\n* the name\n* the BFI reference number\n* the pre–production start date\n  * proof of British certification""",
      List(
        "For each animated television or film, we’ll ask for:",
        "the name",
        "the BFI reference number",
        "the pre–production start date",
        "proof of British certification"
      )
    ),
    (
      """For each animated television or film, we’ll ask for:\n- the name\n- the BFI reference number\n* the pre–production start date\n  - proof of British certification""",
      List(
        "For each animated television or film, we’ll ask for:",
        "the name",
        "the BFI reference number",
        "the pre–production start date",
        "proof of British certification"
      )
    ),
    (
      """you’ll need to provide their:\n\n* name\n\n* address \n\n* date of birth in the format DD MM YYYY \n\n\n\nHMRC may use these details to make checks or contact the participants if we have any questions.""",
      List(
        "you’ll need to provide their:",
        "name",
        "address",
        "date of birth in the format DD MM YYYY",
        "HMRC may use these details to make checks or contact the participants if we have any questions."
      )
    ),
    (
      """you’ll need to provide their:\n\n- name\n\n- address \n\n- date of birth in the format DD MM YYYY \n\n\n\nHMRC may use these details to make checks or contact the participants if we have any questions.""",
      List(
        "you’ll need to provide their:",
        "name",
        "address",
        "date of birth in the format DD MM YYYY",
        "HMRC may use these details to make checks or contact the participants if we have any questions."
      )
    ),
    (
      """you’ll need to provide their:\n\n*name\n\n-address""",
      List(
        "you’ll need to provide their:",
        "*name",
        "-address"
      )
    ),
    (
      """We sent a confirmation email to **${email}** with your submission reference. \n\n[Print or save a PDF copy of your form](${link.printAcknowledgementPdf})\n ## What happens next\n\n We aim to process your application within 15 working days.""",
      List(
        "We sent a confirmation email to **${email}** with your submission reference.",
        "[Print or save a PDF copy of your form](${link.printAcknowledgementPdf})",
        "What happens next",
        "We aim to process your application within 15 working days."
      )
    ),
    (
      "###${name} confirmed that: \n\n <ul> <li>they understand that any money they repay cannot be paid back to them again</li> <li>to the best of their knowledge, the details they have provided are correct</li> </ul>\n\n###Payment details\n\n<table><tbody><tr> <td><b>Sort code</b></td> <td>20 50 46</td> </tr> <tr> <td><b>Account number</b></td> <td>83016684</td> </tr> <tr> <td><b>Account name</b></td> <td>HMRC&nbsp;GOV&nbsp;SCHEMES</td> </tr> <tr> <td><b>Payment reference&nbsp;</b></td> <td>${paymentReference}</td> </tr> </tbody> </table>",
      List(
        "${name} confirmed that:",
        "they understand that any money they repay cannot be paid back to them again",
        "to the best of their knowledge, the details they have provided are correct",
        "Payment details",
        "<b>Sort code</b>",
        "20 50 46",
        "<b>Account number</b>",
        "<b>Account name</b>",
        "HMRC&nbsp;GOV&nbsp;SCHEMES",
        "<b>Payment reference\u00a0</b>"
      )
    ),
    (
      """We sent a confirmation email to **${email}** with your submission reference. The email does not include a copy of your answers. \n\n[Print or save a copy of your form (opens in new tab)](${link.printAcknowledgementPdf})  \n\n ## What happens next \n\nYour return will now be processed.\n\n If we need to discuss your return, we will get in touch using the contact information we hold.\n\n ## Keep your details up-to-date \n\n You must tell the <a href="https://www.gov.uk/government/organisations/hm-revenue-customs/contact/mineral-oils-reliefs">Mineral Oil Reliefs Centre</a> about any changes to the information contained within your RDCO approval.""",
      List(
        "We sent a confirmation email to **${email}** with your submission reference. The email does not include a copy of your answers.",
        "[Print or save a copy of your form (opens in new tab)](${link.printAcknowledgementPdf})",
        "What happens next",
        "Your return will now be processed.",
        "If we need to discuss your return, we will get in touch using the contact information we hold.",
        "Keep your details up-to-date",
        """You must tell the <a href="https://www.gov.uk/government/organisations/hm-revenue-customs/contact/mineral-oils-reliefs">Mineral Oil Reliefs Centre</a> about any changes to the information contained within your RDCO approval."""
      )
    ),
    (
      """\n* Austria \n* Belgium \n* Bulgaria \n* Croatia \n* Republic of Cyprus \n* Czech Republic""",
      List(
        "Austria",
        "Belgium",
        "Bulgaria",
        "Croatia",
        "Republic of Cyprus",
        "Czech Republic"
      )
    ),
    (
      """<p>You must tell us which premises the business will be producing the following in:</p> <ul>${if noBeerSelected then '<li>beer</li>' else ''} ${if noCiderSelected then '<li>cider or perry</li>'  else ''} ${if noWineSelected then '<li>Wine</li>'  else ''} ${if noSpiritSelected then '<li>spirits</li>'  else ''}</ul> <p><a href='${link.addedProductionPremises}'>Return to add these details</a></p>""",
      List(
        "You must tell us which premises the business will be producing the following in:",
        "${if noBeerSelected then '",
        "beer",
        "' else ''} ${if noCiderSelected then '",
        "cider or perry",
        "'  else ''} ${if noWineSelected then '",
        "Wine",
        "'  else ''} ${if noSpiritSelected then '",
        "spirits",
        "'  else ''}",
        """<a href="${link.addedProductionPremises}">Return to add these details</a>"""
      ) // HTML in expressions needs better way of handling.
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
      Map.empty[EnFromSpreadsheet, CyFromSpreadsheet],
      """<p>${abc}</p>""",
      """<p>${abc}</p>"""
    ),
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
        )
      ),
      """\n\nEN1\n\n\nEN2\n   \nEN3""",
      """\n\nCY1\n\n\nCY2\n   \nCY3"""
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
        )
      ),
      """\n\nEN1\n*EN2\n  *EN3""",
      """\n\nCY1\n*CY2\n  *CY3"""
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
        )
      ),
      """\n\nEN1\n-EN2\n  -EN3""",
      """\n\nCY1\n-CY2\n  -CY3"""
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
        )
      ),
      """<div>EN1<ul><li>EN2</li></ul>EN3</div>""",
      """<div>CY1<ul><li>CY2</li></ul>CY3</div>"""
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
          EnFromSpreadsheet("EN4"),
          CyFromSpreadsheet("CY4")
        )
      ),
      """###EN1\n##EN2\n #EN3 \n\n #EN4""",
      """###CY1\n##CY2\n #CY3 \n\n #CY4"""
    )
  ).zipWithIndex.foreach { case ((spreadsheetData, english, expectedWelsh), index) =>
    val spreadsheet = Spreadsheet(spreadsheetData)
    test(s"html translation ${index + 1}") {
      val translateTexts = ExtractAndTranslate(english.stripMargin).translate(spreadsheet)
      assertEquals(translateTexts, expectedWelsh.stripMargin)
    }
  }
}
