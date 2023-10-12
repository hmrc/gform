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

package uk.gov.hmrc.gform.sharedmodel.formtemplate
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.libs.json.{ JsResult, _ }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.formtemplate.FormTemplatesControllerRequestHandler

class FormTemplateJSONSpec extends Spec with TableDrivenPropertyChecks {

  "normaliseJSON" should "ensure default values for missing fields" in {

    val printSectionInput = Json.parse("""|{
                                          |  "_id": "FooBar",
                                          |  "printSection": {
                                          |    "title": "Next Steps",
                                          |    "summaryPdf": "TestSummaryPdf"
                                          |  },
                                          |  "sections": []
                                          |}
      """.stripMargin)

    val summarySection = SummarySection.defaultJson(Default)

    val printSectionExpected = Json.parse(s"""
                                             |{
                                             |  "_id": "foobar",
                                             |  "originalId" : "FooBar",
                                             |  "draftRetrievalMethod": {
                                             |    "showContinueOrDeletePage": "true",
                                             |    "value": "formAccessCodeForAgents"
                                             |  },
                                             |  "formCategory": "default",
                                             |  "languages": [
                                             |    "en"
                                             |  ],
                                             |  "destinations": {
                                             |    "title": "Next Steps",
                                             |    "summaryPdf": "TestSummaryPdf"
                                             |  },
                                             |  "formKind": {
                                             |    "type": "taskList"
                                             |  },
                                             |  "displayHMRCLogo": false,
                                             |  "parentFormSubmissionRefs": [],
                                             |  "summarySection": $summarySection,
                                             |  "allowedFileTypes": [
                                             |    "pdf",
                                             |    "jpg",
                                             |    "xlsx",
                                             |    "ods",
                                             |    "docx",
                                             |    "odt",
                                             |    "pptx",
                                             |    "odp"
                                             |  ],
                                             |  "formKind": {
                                             |    "type": "classic",
                                             |    "sections": []
                                             |  }
                                             |}
      """.stripMargin)

    val destinationsInput = Json.parse("""
                                         |{
                                         |  "_id": "FooBar",
                                         |  "formCategory": "letter",
                                         |  "draftRetrievalMethod": "formAccessCodeForAgents",
                                         |  "showContinueOrDeletePage": "false",
                                         |  "parentFormSubmissionRefs": [
                                         |    "123",
                                         |    "456"
                                         |  ],
                                         |  "sections": [],
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
                                         |  },
                                         |  "declarationSection": {
                                         |      "title": "Declaration",
                                         |      "shortName": "Declaration",
                                         |      "fields": [
                                         |          {
                                         |              "id": "helloD",
                                         |              "type": "text",
                                         |              "label": {
                                         |                  "en": "Hello World",
                                         |                  "cy": "Welsh Hello World"
                                         |              }
                                         |          }
                                         |      ]
                                         |   }
                                         |}
      """.stripMargin)

    val destinationsExpected = Json.parse(s"""
                                             |{
                                             |  "_id": "foobar",
                                             |  "originalId": "FooBar",
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
                                             |        "includeIf": "true",
                                             |        "dmsFormId": "TST123",
                                             |        "customerId": "$${auth.gg}",
                                             |        "classificationType": "BT-NRU-Environmental",
                                             |        "businessArea": "FinanceOpsCorpT"
                                             |      }
                                             |    ],
                                             |    "acknowledgementSection": {
                                             |      "displayFeedbackLink": true,
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
                                             |    },
                                             |    "declarationSection": {
                                             |     "title": "Declaration",
                                             |      "shortName": "Declaration",
                                             |      "fields": [
                                             |          {
                                             |              "id": "helloD",
                                             |              "type": "text",
                                             |              "label": {
                                             |                  "en": "Hello World",
                                             |                  "cy": "Welsh Hello World"
                                             |              }
                                             |          }
                                             |      ]
                                             |   }
                                             |  },
                                             |  "displayHMRCLogo": false,
                                             |  "parentFormSubmissionRefs": [
                                             |    "123",
                                             |    "456"
                                             |  ],
                                             |  "summarySection": $summarySection,
                                             |  "allowedFileTypes": [
                                             |    "pdf",
                                             |    "jpg",
                                             |    "xlsx",
                                             |    "ods",
                                             |    "docx",
                                             |    "odt",
                                             |    "pptx",
                                             |    "odp"
                                             |  ],
                                             |  "formKind": {
                                             |    "type": "classic",
                                             |    "sections": []
                                             |  }
                                             |}
      """.stripMargin)

    val input = Json.parse("""
                             |{
                             |  "_id": "FooBar",
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
                             |  "sections": [],
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
                             |  },
                             |  "declarationSection": {
                             |    "title": "Declaration",
                             |    "shortName": "Declaration",
                             |    "fields": [
                             |      {
                             |        "id": "helloD",
                             |        "type": "text",
                             |        "label": {
                             |          "en": "Hello World",
                             |          "cy": "Welsh Hello World"
                             |        }
                             |      }
                             |    ]
                             |  }
                             |}
                            """.stripMargin)

    val expected = Json.parse(s"""
                                 |{
                                 |  "_id": "foobar",
                                 |  "originalId": "FooBar",
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
                                 |        "includeIf": "true",
                                 |        "dmsFormId": "TST123",
                                 |        "customerId": "$${auth.gg}",
                                 |        "classificationType": "BT-NRU-Environmental",
                                 |        "businessArea": "FinanceOpsCorpT"
                                 |      }
                                 |    ],
                                 |    "acknowledgementSection": {
                                 |      "displayFeedbackLink": true,
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
                                 |    },
                                 |    "declarationSection": {
                                 |      "title": "Declaration",
                                 |      "shortName": "Declaration",
                                 |      "fields": [
                                 |        {
                                 |          "id": "helloD",
                                 |          "type": "text",
                                 |          "label": {
                                 |            "en": "Hello World",
                                 |            "cy": "Welsh Hello World"
                                 |          }
                                 |        }
                                 |      ]
                                 |    }
                                 |  },
                                 |  "displayHMRCLogo": false,
                                 |  "testText": "hello",
                                 |  "testJsonArr": [
                                 |    "en",
                                 |    "cy"
                                 |  ],
                                 |  "parentFormSubmissionRefs": [
                                 |    "123",
                                 |    "456"
                                 |  ],
                                 |  "summarySection": $summarySection,
                                 |  "allowedFileTypes": [
                                 |    "pdf",
                                 |    "jpg",
                                 |    "xlsx",
                                 |    "ods",
                                 |    "docx",
                                 |    "odt",
                                 |    "pptx",
                                 |    "odp"
                                 |  ],
                                 |  "formKind": {
                                 |    "type": "classic",
                                 |    "sections": []
                                 |  }
                                 |}
                                """.stripMargin)

    val inputDraftRetrievalMethodIsNotPermitted = Json.parse("""
                                                               |{
                                                               |  "_id": "FooBar",
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
                                                               |  "draftRetrievalMethod": "notPermitted",
                                                               |  "showContinueOrDeletePage": "false",
                                                               |  "parentFormSubmissionRefs": [
                                                               |    "123",
                                                               |    "456"
                                                               |  ],
                                                               |  "sections": [],
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
                                                               |  },
                                                               |  "declarationSection": {
                                                               |    "title": "Declaration",
                                                               |    "shortName": "Declaration",
                                                               |    "fields": [
                                                               |      {
                                                               |        "id": "helloD",
                                                               |        "type": "text",
                                                               |        "label": {
                                                               |          "en": "Hello World",
                                                               |          "cy": "Welsh Hello World"
                                                               |        }
                                                               |      }
                                                               |    ]
                                                               |  }
                                                               |}
                            """.stripMargin)

    val expectedDraftRetrievalMethodIsNotPermitted = Json.parse(s"""
                                                                   |{
                                                                   |  "_id": "foobar",
                                                                   |  "originalId": "FooBar",
                                                                   |  "draftRetrievalMethod": {
                                                                   |    "showContinueOrDeletePage": "false",
                                                                   |    "value": "notPermitted"
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
                                                                   |        "includeIf": "true",
                                                                   |        "dmsFormId": "TST123",
                                                                   |        "customerId": "$${auth.gg}",
                                                                   |        "classificationType": "BT-NRU-Environmental",
                                                                   |        "businessArea": "FinanceOpsCorpT"
                                                                   |      }
                                                                   |    ],
                                                                   |    "acknowledgementSection": {
                                                                   |      "displayFeedbackLink": true,
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
                                                                   |    },
                                                                   |    "declarationSection": {
                                                                   |      "title": "Declaration",
                                                                   |      "shortName": "Declaration",
                                                                   |      "fields": [
                                                                   |        {
                                                                   |          "id": "helloD",
                                                                   |          "type": "text",
                                                                   |          "label": {
                                                                   |            "en": "Hello World",
                                                                   |            "cy": "Welsh Hello World"
                                                                   |          }
                                                                   |        }
                                                                   |      ]
                                                                   |    }
                                                                   |  },
                                                                   |  "displayHMRCLogo": false,
                                                                   |  "testText": "hello",
                                                                   |  "testJsonArr": [
                                                                   |    "en",
                                                                   |    "cy"
                                                                   |  ],
                                                                   |  "parentFormSubmissionRefs": [
                                                                   |    "123",
                                                                   |    "456"
                                                                   |  ],
                                                                   |  "summarySection": $summarySection,
                                                                   |  "allowedFileTypes": [
                                                                   |    "pdf",
                                                                   |    "jpg",
                                                                   |    "xlsx",
                                                                   |    "ods",
                                                                   |    "docx",
                                                                   |    "odt",
                                                                   |    "pptx",
                                                                   |    "odp"
                                                                   |  ],
                                                                   |  "formKind": {
                                                                   |    "type": "classic",
                                                                   |    "sections": []
                                                                   |  }
                                                                   |}
                                """.stripMargin)

    val t = Table(
      ("input", "expected"),
      (printSectionInput, printSectionExpected),
      (destinationsInput, destinationsExpected),
      (input, expected),
      (inputDraftRetrievalMethodIsNotPermitted, expectedDraftRetrievalMethodIsNotPermitted)
    )

    forAll(t) { case (input, expected) =>
      val result: JsResult[JsValue] = FormTemplatesControllerRequestHandler.normaliseJSON(input)
      result should beJsSuccess(expected)
    }
  }

  it should "return validation error when both destinations and printSection are present" in {

    val input = Json.parse("""
                             |{
                             |  "_id": "FooBar",
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
                             | "printSection": {
                             |    "title": "Next Steps",
                             |    "summaryPdf": "TestSummaryPdf"
                             |  },
                             |  "sections": []
                             |}
      """.stripMargin)

    FormTemplatesControllerRequestHandler.normaliseJSON(input) should be(
      FormTemplatesControllerRequestHandler.onlyOneOfDestinationsAndPrintSection
    )
  }

  it should "return validation error when both destinations and printSection are missing" in {

    val input = Json.parse("""
                             |{
                             |  "_id": "TST123",
                             |  "sections": []
                             |}
      """.stripMargin)

    FormTemplatesControllerRequestHandler.normaliseJSON(input) should be(
      FormTemplatesControllerRequestHandler.onlyOneOfDestinationsAndPrintSection
    )
  }

  it should "return validation error when destinations is present but acknowledgementSection is missing" in {

    val input = Json.parse("""
                             |{
                             |  "_id": "FooBar",
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
                             |  "sections": []
                             |}
      """.stripMargin)

    FormTemplatesControllerRequestHandler.normaliseJSON(input) should be(
      FormTemplatesControllerRequestHandler.mandatoryAcknowledgementForDestinationSection
    )
  }

  it should "return validation error when printSection is present and acknowledgementSection is also present" in {

    val input = Json.parse("""
                             |{
                             |  "_id": "FooBar",
                             |  "printSection": {
                             |    "title": "Next Steps",
                             |    "summaryPdf": "TestSummaryPdf"
                             |  },
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
                             |  },
                             |  "sections": []
                             |}
                           """.stripMargin)

    FormTemplatesControllerRequestHandler.normaliseJSON(input) should be(
      FormTemplatesControllerRequestHandler.avoidAcknowledgementForPrintSection
    )
  }

  it should "return validation error when printSection is present and declarationSection is also present" in {

    val input = Json.parse("""
                             |{
                             |  "_id": "FooBar",
                             |  "printSection": {
                             |    "title": "Next Steps",
                             |    "summaryPdf": "TestSummaryPdf"
                             |  },
                             |  "declarationSection": {
                             |    "title": "Declaration",
                             |    "shortName": "Declaration",
                             |    "fields": [
                             |      {
                             |        "id": "helloD",
                             |        "type": "text",
                             |        "label": {
                             |          "en": "Hello World",
                             |          "cy": "Welsh Hello World"
                             |        }
                             |      }
                             |    ]
                             |  },
                             |  "sections": []
                             |}
                           """.stripMargin)

    FormTemplatesControllerRequestHandler.normaliseJSON(input) should be(
      FormTemplatesControllerRequestHandler.avoidDeclarationForPrintSection
    )
  }

}
