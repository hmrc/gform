/*
 * Copyright 2021 HM Revenue & Customs
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

import org.scalatest.{ FlatSpecLike, Matchers }
import uk.gov.hmrc.gform.formtemplate.FormTemplatePIIRefsHelper.{ PIIDetails, Pos }

class FormTemplatePIIRefsHelperSpec extends FlatSpecLike with Matchers {

  private val formTemplate: String =
    """{
      |  "_id" : "test",
      |  "formName" : "Test",
      |  "sections" : [ {
      |    "title" : "Enter name",
      |    "fields" : [ {
      |      "id" : "name",
      |      "type" : "text",
      |      "label" : "Name",
      |      "format" : "text"
      |    } ]
      |  }, {
      |    "title" : "Enter Email",
      |    "fields" : [ {
      |      "id" : "email",
      |      "type" : "text",
      |      "label" : "Email",
      |      "format" : "text"
      |    } ]
      |  }, {
      |    "title" : "Enter address for ${name + '-' + email}",
      |    "fields" : [ {
      |      "id" : "address",
      |      "type" : "address",
      |      "label" : "Address"
      |    } ]
      |  }, {
      |    "id" : "atl",
      |    "type" : "addToList",
      |    "title" : "Add To List ${name}",
      |    "summaryName" : "owner",
      |    "shortName" : "${name}",
      |    "description" : "${name}",
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
      |        "id" : "nameATL",
      |        "type" : "text",
      |        "label" : "Name ATL",
      |        "format" : "text"
      |      } ]
      |    }, {
      |      "title" : {
      |         "en": "Enter address for ${name} ATL",
      |         "cy": "Rhowch gyfeiriad ar gyfer ${name} ATL"
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
    val result = FormTemplatePIIRefsHelper.getTitlesWithPII(formTemplate, List("name", "email"))
    result shouldBe List(
      PIIDetails(Pos(21, 21), "Enter address for ${name + '-' + email}", List("name", "email")),
      PIIDetails(Pos(30, 30), "Add To List ${name}", List("name")),
      PIIDetails(Pos(50, 51), "Enter address for ${name} ATL", List("name"))
    )
  }
}
