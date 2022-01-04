/*
 * Copyright 2022 HM Revenue & Customs
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

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{ FlatSpecLike, Matchers }
import uk.gov.hmrc.gform.formtemplate.FormTemplatePIIRefsHelper.{ PIIDetails, Pos }

class FormTemplatePIIRefsHelperSpec extends FlatSpecLike with Matchers with TableDrivenPropertyChecks {

  private val formTemplate: String =
    """{
      |  "_id" : "test",
      |  "formName" : "Test",
      |  "sections" : [ {
      |    "title" : "Enter company info",
      |    "fields" : [ {
      |      "id" : "companyName",
      |      "type" : "text",
      |      "label" : "Company Name",
      |      "format" : "text"
      |    }, {
      |      "id" : "year",
      |      "type" : "text",
      |      "label" : "Year",
      |      "format" : "text"
      |    }]
      |  }, {
      |    "title" : "Enter email",
      |    "fields" : [ {
      |      "id" : "email",
      |      "type" : "text",
      |      "label" : "Email",
      |      "format" : "text"
      |    } ]
      |  }, {
      |    "title" : "Enter address for ${companyName + '-' + email}",
      |    "fields" : [ {
      |      "id" : "address",
      |      "type" : "address",
      |      "label" : "Address"
      |    } ]
      |  }, {
      |    "id" : "atl",
      |    "type" : "addToList",
      |    "title" : "Add To List ${year}",
      |    "summaryName" : "owner",
      |    "shortName" : "${nameATL}",
      |    "description" : "${nameATL}",
      |    "addAnotherQuestion" : {
      |      "id" : "ownerFc",
      |      "type" : "choice",
      |      "label" : "So you want to add another name?",
      |      "format" : "yesno",
      |      "infoText" : "Another name"
      |    },
      |    "pages" : [ {
      |      "title" : "Enter name ATL",
      |      "fields" : [ {
      |        "id" : "atlName",
      |        "type" : "text",
      |        "label" : "Name ATL",
      |        "format" : "text"
      |      } ]
      |    }, {
      |      "title" : {
      |         "en": "Enter address for ${atlName} ATL",
      |         "cy": "Rhowch gyfeiriad ar gyfer ${atlName} ATL"
      |      },
      |      "fields" : [ {
      |        "id" : "addressATL",
      |        "type" : "address",
      |        "label" : "Address ATL"
      |      } ]
      |    } ]
      |  } ],
      |  "emailTemplateId" : "confirmation",
      |  "authConfig" : {
      |    "authModule" : "anonymous"
      |  },
      |  "declarationSection" : {
      |    "title" : "Declaration page",
      |    "fields" : [ ]
      |  },
      |  "acknowledgementSection" : {
      |    "title" : "Confirmation page",
      |    "fields" : [ ]
      |  },
      |  "destinations" : [ {
      |    "id" : "hmrcDms",
      |    "type" : "hmrcDms",
      |    "failOnError" : true,
      |    "convertSingleQuotes" : true,
      |    "dmsFormId" : "test",
      |    "customerId" : "${' '}",
      |    "classificationType" : "classificationType",
      |    "businessArea" : "businessArea",
      |    "roboticsXml" : true
      |  } ]
      |}""".stripMargin

  "getTitlesWithPII" should "return all titles with PII fields" in {
    val table = Table(
      ("filters", "expected"),
      (
        List.empty,
        List(
          PIIDetails(Pos(26, 26), "Enter address for ${companyName + '-' + email}", List("companyName", "email")),
          PIIDetails(Pos(35, 35), "Add To List ${year}", List("year")),
          PIIDetails(Pos(55, 56), "Enter address for ${atlName} ATL", List("atlName"))
        )
      ),
      (
        List("name", "email"),
        List(
          PIIDetails(Pos(26, 26), "Enter address for ${companyName + '-' + email}", List("companyName", "email")),
          PIIDetails(Pos(55, 56), "Enter address for ${atlName} ATL", List("atlName"))
        )
      ),
      (
        List("companyName"),
        List(
          PIIDetails(Pos(26, 26), "Enter address for ${companyName + '-' + email}", List("companyName"))
        )
      )
    )

    forAll(table) { (filters, expected) =>
      val result = FormTemplatePIIRefsHelper.getTitlesWithPII(formTemplate, filters)
      result shouldBe expected
    }
  }
}
