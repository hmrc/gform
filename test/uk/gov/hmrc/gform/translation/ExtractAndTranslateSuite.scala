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
      "<table class=\"govuk-table govuk-!-margin-top-4\">\n<caption class=\"govuk-table__caption govuk-table__caption--m\">SME</caption>\n<thead class=\"govuk-table__head\">\n<tr class=\"govuk-table__row\">\n<th></th>\n<th></th>\n</tr>\n</thead>\n<tbody class=\"govuk-table__body\">\n<tr class=\"govuk-table__row\">\n<th scope=\"row\" class=\"govuk-table__cell\">Project qualifying expenditure so far</th>\n<td class=\"govuk-table__cell govuk-table__cell--numeric\">${projectTotalNullSme}</td>\n</tr>\n<tr class=\"govuk-table__row\">\n<th scope=\"row\" class=\"govuk-table__header\">Total project amount must account for at least</th>\n<td class=\"govuk-table__cell govuk-table__cell--numeric\">${fiftyOnePerCentSme}</td>\n</tr>\n </tbody>\n</table>",
      List(
        "SME",
        "Project qualifying expenditure so far",
        "Total project amount must account for at least"
      )
    ),
    (
      "<table class='govuk-table'><thead class='govuk-table__head'><tr class='govuk-table__row'><th scope='col' class='govuk-table__header'>Type of item</th><th scope='col' class='govuk-table__header'>Number of this type of item</th></tr></thead><tbody class='govuk-table__body'><tr class='govuk-table__row'><th scope='row' class='govuk-table__header'>TV</th><td class='govuk-table__cell'>2</td></tr><tr class='govuk-table__row'><th scope='row' class='govuk-table__header'>Tablet</th><td class='govuk-table__cell'>3</td></tr><tr class='govuk-table__row'><th scope='row' class='govuk-table__header'>Laptop</th><td class='govuk-table__cell'>1</td></tr></tbody><tbody class='govuk-table__body'><tr class='govuk-table__row'><th scope='row' class='govuk-table__header'>Clothing</th><td class='govuk-table__cell'>55</td></tr><tr class='govuk-table__row'><th scope='row' class='govuk-table__header'>Books</th><td class='govuk-table__cell'>110 (approximately)</td></tr><tr class='govuk-table__row'><th scope='row' class='govuk-table__header'>Cutlery</th><td class='govuk-table__cell'>40 (approximately)</td></tr><tr class='govuk-table__row'><th scope='row' class='govuk-table__header'>Crockery </th><td class='govuk-table__cell'>30 (approximately)</td></tr><tr class='govuk-table__row'><th scope='row' class='govuk-table__header'>Utensils </th><td class='govuk-table__cell'>25 (approximately)</td></tr></tbody></table> \n\n You do not need to include original costs of items, current values, or brands.",
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
      List("EN1", "EN2", "EN3", "EN4")
    ),
    (
      """|<p>To be eligible for R&D tax relief</p>
         |<p><span>contracted out R&D</span></p>""",
      List(
        "To be eligible for R&D tax relief",
        "contracted out R&D"
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
        "To be eligible for R&D tax relief there must be qualifying expenditure in one of these categories.",
        "Links all open in a new tab:",
        """<a href="https://cird84000">externally provided workers</a>""",
        """<a href="https://cird84200">contracted out R&D</a>""",
        """<a class="govuk-link" href="https://cird83000">staffing</a>""",
        """<a href="https://cird82500">software</a>""",
        """<a href="https://CIRD135000">cloud computing</a>""",
        """Next you'll need to tell us which categories incurred qualifying expenditure, and how much was spent. &nbsp; &nbsp;"""
      )
    ),
    (
      "<h3 class='govuk-heading-l'>Amount to pay now: ${grandTotal}</h3>\n\nYou need to use the banking details below. Save this information for your records. It cannot be emailed to you for security reasons.",
      List(
        "Amount to pay now: ${grandTotal}",
        "You need to use the banking details below.",
        "Save this information for your records.",
        "It cannot be emailed to you for security reasons."
      )
    ),
    (
      "You need to send us all of the relevant forms within **24 hours**.\n\nIf you do not send us these details in time, you will need to start a new application.\n\nWhen you are ready to submit all of your forms, you will need to [return to each of your saved forms](/submissions/new-form/AEO-journey-selector) to submit them.\n\nIf you have sent us all the relevant details, we will tell you within 90 calendar days if your application has been successful.\n\nIf your application is unsuccessful we will tell you why.",
      List(
        "You need to send us all of the relevant forms within **24 hours**.",
        "If you do not send us these details in time, you will need to start a new application.",
        "When you are ready to submit all of your forms, you will need to [return to each of your saved forms](/submissions/new-form/AEO-journey-selector) to submit them.",
        "If you have sent us all the relevant details, we will tell you within 90 calendar days if your application has been successful.",
        "If your application is unsuccessful we will tell you why."
      )
    ),
    (
      "On the next pages we will ask you some details, including: <ul><li>where you live</li><li>if you're eligible for this allowance</li><li>when you were registered as blind (for England and Wales)</li><li>the local authority you are registered blind with (for England and Wales)</li><li>the allowance amount</li></ul> \n\n<div class=\"govuk-inset-text\">\nYou can only tell us about one tax year at a time.\n</div>",
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
      "##Blind Person’s allowance\n\n\nYou can tell us if: <ul><li>the allowance is missing from your Personal tax account </li><li>you no longer receive this allowance</li></ul>\n\n",
      List(
        "Blind Person’s allowance",
        "You can tell us if:",
        "the allowance is missing from your Personal tax account",
        "you no longer receive this allowance"
      )
    ),
    (
      "###What has changed \n \n You can now operate PAYE on a proportion of the employee’s income.",
      List(
        "What has changed",
        "You can now operate PAYE on a proportion of the employee’s income."
      )
    ),
    (
      "For each animated television or film, we’ll ask for:\n* the name\n* the BFI reference number\n* the pre–production start date\n  * proof of British certification",
      List(
        "For each animated television or film, we’ll ask for:",
        "the name",
        "the BFI reference number",
        "the pre–production start date",
        "proof of British certification"
      )
    ),
    (
      "For each animated television or film, we’ll ask for:\n- the name\n- the BFI reference number\n* the pre–production start date\n  - proof of British certification",
      List(
        "For each animated television or film, we’ll ask for:",
        "the name",
        "the BFI reference number",
        "the pre–production start date",
        "proof of British certification"
      )
    ),
    (
      "you’ll need to provide their:\n\n* name\n\n* address \n\n* date of birth in the format DD MM YYYY \n\n\n\nHMRC may use these details to make checks or contact the participants if we have any questions.",
      List(
        "you’ll need to provide their:",
        "name",
        "address",
        "date of birth in the format DD MM YYYY",
        "HMRC may use these details to make checks or contact the participants if we have any questions."
      )
    ),
    (
      "you’ll need to provide their:\n\n- name\n\n- address \n\n- date of birth in the format DD MM YYYY \n\n\n\nHMRC may use these details to make checks or contact the participants if we have any questions.",
      List(
        "you’ll need to provide their:",
        "name",
        "address",
        "date of birth in the format DD MM YYYY",
        "HMRC may use these details to make checks or contact the participants if we have any questions."
      )
    ),
    (
      "you’ll need to provide their:\n\n*name\n\n-address",
      List(
        "you’ll need to provide their:",
        "*name",
        "-address"
      )
    ),
    (
      "We sent a confirmation email to **${email}** with your submission reference. \n\n[Print or save a PDF copy of your form](${link.printAcknowledgementPdf})\n ## What happens next\n\n We aim to process your application within 15 working days.",
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
        "Sort code",
        "Account number",
        "Account name",
        "HMRC&nbsp;GOV&nbsp;SCHEMES",
        "Payment reference&nbsp;"
      )
    ),
    (
      "We sent a confirmation email to **${email}** with your submission reference. The email does not include a copy of your answers. \n\n[Print or save a copy of your form (opens in new tab)](${link.printAcknowledgementPdf})  \n\n ## What happens next \n\nYour return will now be processed.\n\n If we need to discuss your return, we will get in touch using the contact information we hold.\n\n ## Keep your details up-to-date \n\n You must tell the <a href=\"https://www.gov.uk/government/organisations/hm-revenue-customs/contact/mineral-oils-reliefs\">Mineral Oil Reliefs Centre</a> about any changes to the information contained within your RDCO approval.",
      List(
        "We sent a confirmation email to **${email}** with your submission reference.",
        "The email does not include a copy of your answers.",
        "[Print or save a copy of your form (opens in new tab)](${link.printAcknowledgementPdf})",
        "What happens next",
        "Your return will now be processed.",
        "If we need to discuss your return, we will get in touch using the contact information we hold.",
        "Keep your details up-to-date",
        """You must tell the <a href="https://www.gov.uk/government/organisations/hm-revenue-customs/contact/mineral-oils-reliefs">Mineral Oil Reliefs Centre</a> about any changes to the information contained within your RDCO approval."""
      )
    ),
    (
      "\n* Austria \n* Belgium \n* Bulgaria \n* Croatia \n* Republic of Cyprus \n* Czech Republic",
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
      "* general accounting and\npayments\n* sales, orders, purchases and purchase orders\n* inventory control and storage\n* manufacture",
      List(
        "general accounting and\npayments",
        "sales, orders, purchases and purchase orders",
        "inventory control and storage",
        "manufacture"
      )
    ),
    (
      "*general accounting and",
      List(
        "*general accounting and"
      )
    ),
    (
      " - foo\n- bar\n- baz",
      List(
        "foo",
        "bar",
        "baz"
      )
    ),
    (
      "foo - bar\n- baz",
      List(
        "foo - bar",
        "baz"
      )
    ),
    (
      "<p>You must tell us which premises the business will be producing the following in:</p> <ul>${if noBeerSelected then '<li>beer</li>' else ''} ${if noCiderSelected then '<li>cider or perry</li>'  else ''} ${if noWineSelected then '<li>Wine</li>'  else ''} ${if noSpiritSelected then '<li>spirits</li>'  else ''}</ul> <p><a href='${link.addedProductionPremises}'>Return to add these details</a></p>",
      List(
        "You must tell us which premises the business will be producing the following in:",
        "${if noBeerSelected then '<li>beer</li>' else ''} ${if noCiderSelected then '<li>cider or perry</li>'  else ''} ${if noWineSelected then '<li>Wine</li>'  else ''} ${if noSpiritSelected then '<li>spirits</li>'  else ''}",
        """<a href="${link.addedProductionPremises}">Return to add these details</a>"""
      )
    ),
    (
      "<ul class='govuk-list govuk-list--bullet'><li style='${if numberVideoGames = 0 then 'display:none' else ''}'>${numberVideoGames} video game(s)</li><li style='${if numberFilms = 0 then 'display:none' else ''}'>${numberFilms} film(s)</li></ul>",
      List(
        "${numberVideoGames} video game(s)",
        "${numberFilms} film(s)"
      )
    ),
    (
      "You must tell ${if (isAgent) then 'us' else 'them'}",
      List(
        "You must tell ${if (isAgent) then 'us' else 'them'}"
      )
    ),
    (
      "## What happens next\n\n ${if consentToEmailChoice contains 1 then 'We will now consider your application.\n\n We aim to get in touch within 30 days.' else concat('We will now consider your application.\n\n We aim to get in touch within 60 days.', contactEmail, '')}",
      List(
        "What happens next",
        "${if consentToEmailChoice contains 1 then 'We will now consider your application.\n\n We aim to get in touch within 30 days.' else concat('We will now consider your application.\n\n We aim to get in touch within 60 days.', contactEmail, '')}"
      )
    ),
    (
      "This means that ${businessName} made money from:\n\n* doing business (‘trading profits’)\n\n* investments\n\n* selling assets for more than they cost (‘chargeable gains’)",
      List(
        "This means that ${businessName} made money from:",
        "doing business (‘trading profits’)",
        "investments",
        "selling assets for more than they cost (‘chargeable gains’)"
      )
    ),
    (
      "${if consent contains 0 then 'We will only use this.' else 'We will only use that.'}",
      List("${if consent contains 0 then 'We will only use this.' else 'We will only use that.'}")
    ),
    (
      "${if (ccHasQe contains 0) then 'foo'|'bar' else 0}",
      List.empty[String] // Nothing to translate
    ),
    (
      "${if (ccHasQe contains 0) then ccTotalQualifyingExpenditure else 0}",
      List.empty[String] // Nothing to translate
    ),
    (
      "**${additionalDuty}**<br> Paid: ${hideZeroDecimals(whatadditionalDutyPaid)}<br>Should have paid: ${hideZeroDecimals(aditionalDutyPaidShouldHavePaid)}<br>Estimated amount due: ${hideZeroDecimals(additionalDutyOvepayAmount)}",
      List(
        "Paid: ${hideZeroDecimals(whatadditionalDutyPaid)}",
        "Should have paid: ${hideZeroDecimals(aditionalDutyPaidShouldHavePaid)}",
        "Estimated amount due: ${hideZeroDecimals(additionalDutyOvepayAmount)}"
      )
    ),
    (
      "##2. Worked example of the proposed PESM",
      List("2. Worked example of the proposed PESM")
    ),
    (
      "1. Open a new tab or window. \n2. Go to GOV.UK. \n3. Search for 'Rates for Air Passenger Duty'.",
      List(
        "1. Open a new tab or window.",
        "2. Go to GOV.UK.",
        "3. Search for 'Rates for Air Passenger Duty'."
      )
    ),
    (
      "1. Open a new tab or window. \n2. Go to GOV.UK. \n3.Search for 'Rates for Air Passenger Duty'.",
      List(
        "1. Open a new tab or window.",
        "2. Go to GOV.UK. \n3.Search for 'Rates for Air Passenger Duty'."
      )
    ),
    (
      "1. Open a new tab or window. \n2.Go to GOV.UK. \n3.Search for 'Rates for Air Passenger Duty'.\n4. And then go back one step.",
      List(
        "1. Open a new tab or window. \n2.Go to GOV.UK. \n3.Search for 'Rates for Air Passenger Duty'.",
        "4. And then go back one step."
      )
    ),
    (
      "1. Open a new\ntab or window. \n2. Go to\nGOV.UK. \n3. Search for\n'Rates for Air Passenger Duty'.",
      List(
        "1. Open a new\ntab or window.",
        "2. Go to\nGOV.UK.",
        "3. Search for\n'Rates for Air Passenger Duty'."
      )
    ),
    (
      (
        "We will use these for your ongoing\n correspondence and returns for duty on gas for use as road fuel.",
        List(
          "We will use these for your ongoing\n correspondence and returns for duty on gas for use as road fuel."
        )
      )
    ),
    (
      "You can upload scanned copies or photos. We will accept JPEG or PDF files. The maximum file size is 10MB. The available qualifying expenditure for that period (i.e. the lesser of core UK expenditure or 80% of the total core expenditure, less any amount surrendered in previous periods)",
      List(
        "You can upload scanned copies or photos.",
        "We will accept JPEG or PDF files.",
        "The maximum file size is 10MB.",
        "The available qualifying expenditure for that period (i.e. the lesser of core UK expenditure or 80% of the total core expenditure, less any amount surrendered in previous periods)"
      )
    ),
    (
      "###What happens next\nWe’ll send an email to you.\nCheck your spam folder.",
      List(
        "What happens next\nWe’ll send an email to you.\nCheck your spam folder."
      )
    ),
    (
      "This file has already been uploaded. Select a different file.",
      List(
        "This file has already been uploaded.",
        "Select a different file."
      )
    ),
    (
      "${if isHMRCEnrolledAgent then 'You’re logged in as an agent. ' else ''}We need to know your trading name, whether self employed or working for a tax agency or consultancy. If you trade under your own name, enter your name.",
      List(
        "${if isHMRCEnrolledAgent then 'You’re logged in as an agent. ' else ''}We need to know your trading name, whether self employed or working for a tax agency or consultancy.",
        "If you trade under your own name, enter your name."
      )
    ),
    (
      "**$n**. Invoice ref: ${invoiceNumber} VAT amount: ${vatAmount}",
      List("Invoice ref: ${invoiceNumber} VAT amount: ${vatAmount}")
    ),
    (
      "**$n**. ${otherClaimantName}",
      Nil
    ),
    (
      "<b>Account name</b>",
      List(
        "Account name"
      )
    ),
    (
      "<span><b>Account name</b></span>",
      List(
        "<span><b>Account name</b></span>"
      )
    ),
    (
      "<strong>SEIS full time-employees</strong> \n\nTo qualify for SEIS investment, the company must have fewer than 25 FTE employees immediately before share issue. Because you told us the company will have 25 or more, a caseworker will review this.",
      List(
        "SEIS full time-employees",
        "To qualify for SEIS investment, the company must have fewer than 25 FTE employees immediately before share issue.",
        "Because you told us the company will have 25 or more, a caseworker will review this."
      )
    ),
    (
      "<strong>Accounting period $n</strong>   <br>${accountingPeriod}<br>Amount: ${taxAmountOwedCalculated orElse amountOfferedVR} <br> <span class='${if isVoluntaryRestitution then 'govuk-tag'  else '' }'>${if isVoluntaryRestitution then 'Voluntary payment'  else '' }</span>",
      List(
        "Accounting period $n",
        "Amount: ${taxAmountOwedCalculated orElse amountOfferedVR}",
        "${if isVoluntaryRestitution then 'Voluntary payment'  else '' }"
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
          EnFromSpreadsheet("Inline element"),
          CyFromSpreadsheet("CY1")
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
          EnFromSpreadsheet("EN4"),
          CyFromSpreadsheet("CY4")
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
            "To be eligible for R&D tax relief there must be qualifying expenditure in one of these categories."
          ),
          CyFromSpreadsheet("CY1-A R&D CY1-B.")
        ),
        (
          EnFromSpreadsheet("Links all open in a new tab:"),
          CyFromSpreadsheet("CY1-C")
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
      """|CY1-A R&D CY1-B. CY1-C
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
      "\n\nEN1\n\n\nEN2\n   \nEN3",
      "\n\nCY1\n\n\nCY2\n   \nCY3"
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
      "\n\nEN1\n*EN2\n  *EN3",
      "\n\nCY1\n*CY2\n  *CY3"
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
      "\n\nEN1\n-EN2\n  -EN3",
      "\n\nCY1\n-CY2\n  -CY3"
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
      "<div>EN1<ul><li>EN2</li></ul>EN3</div>",
      "<div>CY1<ul><li>CY2</li></ul>CY3</div>"
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
      "###EN1\n##EN2\n #EN3 \n\n #EN4",
      "###CY1\n##CY2\n #CY3 \n\n #CY4"
    ),
    (
      Map(
        (
          EnFromSpreadsheet("${numberVideoGames} video game(s)"),
          CyFromSpreadsheet("${numberVideoGames} CY1")
        ),
        (EnFromSpreadsheet("${numberFilms} film(s)"), CyFromSpreadsheet("${numberFilms} CY2"))
      ),
      "<ul class='govuk-list govuk-list--bullet'><li style='${if numberVideoGames = 0 then 'display:none' else ''}'>${numberVideoGames} video game(s)</li><li style='${if numberFilms = 0 then 'display:none' else ''}'>${numberFilms} film(s)</li></ul>",
      "<ul class=\"govuk-list govuk-list--bullet\"><li style=\"${if numberVideoGames = 0 then 'display:none' else ''}\">${numberVideoGames} CY1</li><li style=\"${if numberFilms = 0 then 'display:none' else ''}\">${numberFilms} CY2</li></ul>"
    ),
    (
      Map(
        (
          EnFromSpreadsheet("This means that ${businessName} made money from:"),
          CyFromSpreadsheet("CY1")
        ),
        (
          EnFromSpreadsheet("doing business (‘trading profits’)"),
          CyFromSpreadsheet("CY2")
        ),
        (
          EnFromSpreadsheet("investments"),
          CyFromSpreadsheet("CY3")
        ),
        (
          EnFromSpreadsheet("selling assets for more than they cost (‘chargeable gains’)"),
          CyFromSpreadsheet("CY4")
        )
      ),
      "This means that ${businessName} made money from:\n\n* doing business (‘trading profits’)\n\n* investments\n\n* selling assets for more than they cost (‘chargeable gains’)",
      "CY1\n\n* CY2\n\n* CY3\n\n* CY4"
    ),
    (
      Map(
        (EnFromSpreadsheet("Paid: ${hideZeroDecimals(whatadditionalDutyPaid)}"), CyFromSpreadsheet("CY2")),
        (
          EnFromSpreadsheet("Should have paid: ${hideZeroDecimals(aditionalDutyPaidShouldHavePaid)}"),
          CyFromSpreadsheet("CY3")
        ),
        (
          EnFromSpreadsheet("Estimated amount due: ${hideZeroDecimals(additionalDutyOvepayAmount)}"),
          CyFromSpreadsheet("CY4")
        )
      ),
      "**${additionalDuty}**<br> Paid: ${hideZeroDecimals(whatadditionalDutyPaid)}<br>Should have paid: ${hideZeroDecimals(aditionalDutyPaidShouldHavePaid)}<br>Estimated amount due: ${hideZeroDecimals(additionalDutyOvepayAmount)}",
      "**${additionalDuty}**<br> CY2<br>CY3<br>CY4"
    ),
    (
      Map(
        (EnFromSpreadsheet("SEIS full time-employees"), CyFromSpreadsheet("CY1")),
        (
          EnFromSpreadsheet(
            "To qualify for SEIS investment, the company must have fewer than 25 FTE employees immediately before share issue."
          ),
          CyFromSpreadsheet("CY2.")
        ),
        (
          EnFromSpreadsheet(
            "Because you told us the company will have 25 or more, a caseworker will review this."
          ),
          CyFromSpreadsheet("CY3.")
        )
      ),
      "<strong>SEIS full time-employees</strong> \n\nTo qualify for SEIS investment, the company must have fewer than 25 FTE employees immediately before share issue. Because you told us the company will have 25 or more, a caseworker will review this.",
      "<strong>CY1</strong> \n\nCY2. CY3."
    ),
    (
      Map(
        (EnFromSpreadsheet("Accounting period $n"), CyFromSpreadsheet("CY1")),
        (EnFromSpreadsheet("Amount: ${taxAmountOwedCalculated orElse amountOfferedVR}"), CyFromSpreadsheet("CY2")),
        (EnFromSpreadsheet("${if isVoluntaryRestitution then 'Voluntary payment'  else '' }"), CyFromSpreadsheet("CY3"))
      ),
      "<strong>Accounting period $n</strong>   <br>${accountingPeriod}<br>Amount: ${taxAmountOwedCalculated orElse amountOfferedVR} <br> <span class='${if isVoluntaryRestitution then 'govuk-tag'  else '' }'>${if isVoluntaryRestitution then 'Voluntary payment'  else '' }</span>",
      "<strong>CY1</strong>   <br>${accountingPeriod}<br>CY2 <br> <span class=\"${if isVoluntaryRestitution then 'govuk-tag'  else '' }\">CY3</span>"
    )
  ).zipWithIndex.foreach { case ((spreadsheetData, english, expectedWelsh), index) =>
    val spreadsheet = Spreadsheet(spreadsheetData)
    test(s"html translation ${index + 1}") {
      val translateTexts = ExtractAndTranslate(english.stripMargin).translate(spreadsheet)
      assertEquals(translateTexts, expectedWelsh.stripMargin)
    }
  }

  List(
    (
      "identity",
      List(
        "identity"
      )
    ),
    (
      "\nYou can only tell us about one tax year at a time.\n",
      List(
        "\nYou can only tell us about one tax year at a time.\n"
      )
    ),
    (
      "\nFoo.\n1 Bar.\n",
      List(
        "\nFoo.\n1 Bar.\n"
      )
    ),
    (
      "\nFoo.\n1. Bar.\n",
      List(
        "\nFoo.",
        "1. Bar.\n"
      )
    ),
    (
      "\nFoo.\n1. Bar\nBaz.\n",
      List(
        "\nFoo.",
        "1. Bar\nBaz.\n"
      )
    ),
    (
      "\nFoo.\n123. Bar\nBaz.\n",
      List(
        "\nFoo.",
        "123. Bar\nBaz.\n"
      )
    ),
    (
      "\n1. Foo.\n2. Bar.\n3. Baz.",
      List(
        "1. Foo.",
        "2. Bar.",
        "3. Baz."
      )
    ),
    (
      "1. Foo.\n2. Bar.\n3. Baz.",
      List(
        "1. Foo.",
        "2. Bar.",
        "3. Baz."
      )
    )
  ).zipWithIndex.foreach { case ((english, expected), index) =>
    test(s"splitOrderedList ${index + 1}") {
      val output = ExtractAndTranslate.splitOrderedList(english)

      assertEquals(output, expected)

    }
  }

  List(
    "**$n**.",
    "**$n. ${accompanyingUploadTitle}**",
    "**$n.** ${customerNamePersonOwner orElse customerNameOwnerEntity}",
    "**$n. ** ${alcoholProductProducerName}: ${producersAPPAID}",
    "${loansAction} $n",
    "${accountFirstName} ${accountLastName}",
    "${accountFirstName}: ${accountLastName}",
    "${rate}%",
    "${''}",
    "${concat(accountFirstName,' ',accountLastName)}",
    "${dateOstartSelfEmploymentAtl} ${selfEmployerStartDateAtl} – ${selfEmployerEndDateAtl} ${currentJobz}",
    "**$n. ${documentType}** ${if (documentType contains 'other') then ': ' else ''} ${documentDescription}"
  ).zipWithIndex.foreach { case (english, index) =>
    test(s"nothingToTranslate positive ${index + 1}") {
      val output = ExtractAndTranslate.nothingToTranslate(english)
      assert(clue(output), s"Failed on $english")
    }
  }

  List(
    "${managingPartnerFullName}'s",
    "${nineLastTaxYear} to ${eightLastTaxYear}",
    "${country} ${if choiceBeforeJan2021 contains 0 then '- national before January 2021' else if choiceBeforeJan2021 contains 1 then '- national after January 2021' else ''}",
    """${if consentToEmailChoice contains 1 then 'We will now consider your application.\n\n We aim to get in touch within 30 days.' else concat('We will now consider your application.\n\n We aim to get in touch within 60 days.', contactEmail, '')}"""
  ).zipWithIndex.foreach { case (english, index) =>
    test(s"nothingToTranslate negative ${index + 1}") {
      val output = !ExtractAndTranslate.nothingToTranslate(english)
      assert(clue(output), s"Failed on $english")
    }
  }
}
