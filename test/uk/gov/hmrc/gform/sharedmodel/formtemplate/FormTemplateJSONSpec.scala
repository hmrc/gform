/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate
import play.api.libs.json._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.formtemplate.FormTemplatesControllerRequestHandler

class FormTemplateJSONSpec extends Spec {

  "normaliseJSON" should "ensure default values for missing fields" in {

    val printSectionInput = Json.parse("""
                                         |{
                                         |  "printSection": "TestPrintSection"
                                         |}
      """.stripMargin)

    val printSectionExpected = Json.parse("""
                                            |{
                                            |  "draftRetrievalMethod": {
                                            |    "showContinueOrDeletePage": "true",
                                            |    "value": "onePerUser"
                                            |  },
                                            |  "formCategory": "default",
                                            |  "languages": [
                                            |    "en"
                                            |  ],
                                            |  "destinations": "TestPrintSection",
                                            |  "parentFormSubmissionRefs": []
                                            |}
      """.stripMargin)

    val destinationsInput = Json.parse("""
                                         |{
                                         |  "formCategory": "letter",
                                         |  "draftRetrievalMethod": "formAccessCodeForAgents",
                                         |  "showContinueOrDeletePage": "false",
                                         |  "parentFormSubmissionRefs": [
                                         |    "123",
                                         |    "456"
                                         |  ],
                                         |  "destinations": [
                                         |    {
                                         |      "id": "HMRCDMS",
                                         |      "type": "hmrcDms",
                                         |      "dmsFormId": "TST123",
                                         |      "customerId": "${auth.gg}",
                                         |      "classificationType": "BT-NRU-Environmental",
                                         |      "businessArea": "FinanceOpsCorpT"
                                         |    }
                                         |  ],
                                         |  "acknowledgementSection": {
                                         |    "shortName": "Acknowledgement Page",
                                         |    "title": "Acknowledgement Page",
                                         |    "fields": [
                                         |      {
                                         |        "type": "info",
                                         |        "id": "ackpageInfo",
                                         |        "label": "SomeContent",
                                         |        "infoText": "SomeContent"
                                         |      }
                                         |    ]
                                         |  }
                                         |}
      """.stripMargin)

    val destinationsExpected = Json.parse("""
                                            |{
                                            |  "draftRetrievalMethod": {
                                            |    "showContinueOrDeletePage": "false",
                                            |    "value": "formAccessCodeForAgents"
                                            |  },
                                            |  "formCategory": "letter",
                                            |  "languages": [
                                            |    "en"
                                            |  ],
                                            |  "destinations": {
                                            |    "destinations": [
                                            |      {
                                            |        "id": "HMRCDMS",
                                            |        "type": "hmrcDms",
                                            |        "dmsFormId": "TST123",
                                            |        "customerId": "${auth.gg}",
                                            |        "classificationType": "BT-NRU-Environmental",
                                            |        "businessArea": "FinanceOpsCorpT"
                                            |      }
                                            |    ],
                                            |    "acknowledgementSection": {
                                            |      "shortName": "Acknowledgement Page",
                                            |      "title": "Acknowledgement Page",
                                            |      "fields": [
                                            |        {
                                            |          "type": "info",
                                            |          "id": "ackpageInfo",
                                            |          "label": "SomeContent",
                                            |          "infoText": "SomeContent"
                                            |        }
                                            |      ]
                                            |    }
                                            |  },
                                            |  "parentFormSubmissionRefs": [
                                            |    "123",
                                            |    "456"
                                            |  ]
                                            |}
      """.stripMargin)

    val input = Json.parse("""
                             |{
                             |  "testText": "hello",
                             |  "testJsonObj": {
                             |    "id": "transitionToSubmitted",
                             |    "type": "stateTransition",
                             |    "requiredState": "Submitted"
                             |  },
                             |  "testJsonArr": [
                             |    "en",
                             |    "cy"
                             |  ],
                             |  "formCategory": "letter",
                             |  "draftRetrievalMethod": "formAccessCodeForAgents",
                             |  "showContinueOrDeletePage": "false",
                             |  "parentFormSubmissionRefs": [
                             |    "123",
                             |    "456"
                             |  ],
                             |  "destinations": [
                             |    {
                             |      "id": "HMRCDMS",
                             |      "type": "hmrcDms",
                             |      "dmsFormId": "TST123",
                             |      "customerId": "${auth.gg}",
                             |      "classificationType": "BT-NRU-Environmental",
                             |      "businessArea": "FinanceOpsCorpT"
                             |    }
                             |  ],
                             |  "acknowledgementSection": {
                             |    "shortName": "Acknowledgement Page",
                             |    "title": "Acknowledgement Page",
                             |    "fields": [
                             |      {
                             |        "type": "info",
                             |        "id": "ackpageInfo",
                             |        "label": "SomeContent",
                             |        "infoText": "SomeContent"
                             |      }
                             |    ]
                             |  }
                             |}
      """.stripMargin)

    val expected = Json.parse("""
                                |{
                                |  "draftRetrievalMethod": {
                                |    "showContinueOrDeletePage": "false",
                                |    "value": "formAccessCodeForAgents"
                                |  },
                                |  "formCategory": "letter",
                                |  "languages": [
                                |    "en"
                                |  ],
                                |  "testJsonObj": {
                                |    "id": "transitionToSubmitted",
                                |    "type": "stateTransition",
                                |    "requiredState": "Submitted"
                                |  },
                                |  "destinations": {
                                |    "destinations": [
                                |      {
                                |        "id": "HMRCDMS",
                                |        "type": "hmrcDms",
                                |        "dmsFormId": "TST123",
                                |        "customerId": "${auth.gg}",
                                |        "classificationType": "BT-NRU-Environmental",
                                |        "businessArea": "FinanceOpsCorpT"
                                |      }
                                |    ],
                                |    "acknowledgementSection": {
                                |      "shortName": "Acknowledgement Page",
                                |      "title": "Acknowledgement Page",
                                |      "fields": [
                                |        {
                                |          "type": "info",
                                |          "id": "ackpageInfo",
                                |          "label": "SomeContent",
                                |          "infoText": "SomeContent"
                                |        }
                                |      ]
                                |    }
                                |  },
                                |  "testText": "hello",
                                |  "testJsonArr": [
                                |    "en",
                                |    "cy"
                                |  ],
                                |  "parentFormSubmissionRefs": [
                                |    "123",
                                |    "456"
                                |  ]
                                |}
      """.stripMargin)

    val t = Table(
      ("input", "expected"),
      (printSectionInput, printSectionExpected),
      (destinationsInput, destinationsExpected),
      (input, expected)
    )

    forAll(t) {
      case (input, expected) =>
        val result = FormTemplatesControllerRequestHandler.normaliseJSON(input)
        result should beJsSuccess(expected)
    }
  }

  it should "return validation error when both destinations and printSection are present" in {

    val input = Json.parse("""
                             |{
                             |  "destinations": [
                             |    {
                             |      "id": "HMRCDMS",
                             |      "type": "hmrcDms",
                             |      "dmsFormId": "TST123",
                             |      "customerId": "${auth.gg}",
                             |      "classificationType": "BT-NRU-Environmental",
                             |      "businessArea": "FinanceOpsCorpT"
                             |    }
                             |  ],
                             | "printSection": "TestPrintSection"
                             |}
      """.stripMargin)

    FormTemplatesControllerRequestHandler.normaliseJSON(input) should be(
      FormTemplatesControllerRequestHandler.onlyOneOfDestinationsAndPrintSection)
  }

  it should "return validation error when both destinations and printSection are missing" in {

    val input = Json.parse("""
                             |{
                             |  "_id": "TST123"
                             |}
      """.stripMargin)

    FormTemplatesControllerRequestHandler.normaliseJSON(input) should be(
      FormTemplatesControllerRequestHandler.onlyOneOfDestinationsAndPrintSection)
  }

  it should "return validation error when destinations is present but acknowledgementSection is missing" in {

    val input = Json.parse("""
                             |{
                             |  "formCategory": "letter",
                             |  "draftRetrievalMethod": "formAccessCodeForAgents",
                             |  "showContinueOrDeletePage": "false",
                             |  "parentFormSubmissionRefs": [
                             |    "123",
                             |    "456"
                             |  ],
                             |  "destinations": [
                             |    {
                             |      "id": "HMRCDMS",
                             |      "type": "hmrcDms",
                             |      "dmsFormId": "TST123",
                             |      "customerId": "${auth.gg}",
                             |      "classificationType": "BT-NRU-Environmental",
                             |      "businessArea": "FinanceOpsCorpT"
                             |    }
                             |  ]
                             |}
      """.stripMargin)

    FormTemplatesControllerRequestHandler.normaliseJSON(input) should be(
      FormTemplatesControllerRequestHandler.mandatoryAcknowledgementForDestinationSection)
  }

  it should "return validation error when printSection is present and acknowledgementSection is also present" in {

    val input = Json.parse("""
                             |{
                             |  "printSection": "TestPrintSection",
                             |  "acknowledgementSection": {
                             |    "shortName": "Acknowledgement Page",
                             |    "title": "Acknowledgement Page",
                             |    "fields": [
                             |      {
                             |        "type": "info",
                             |        "id": "ackpageInfo",
                             |        "label": "SomeContent",
                             |        "infoText": "SomeContent"
                             |      }
                             |    ]
                             |  }
                             |}
                           """.stripMargin)

    FormTemplatesControllerRequestHandler.normaliseJSON(input) should be(
      FormTemplatesControllerRequestHandler.avoidAcknowledgementForPrintSection)
  }

}
