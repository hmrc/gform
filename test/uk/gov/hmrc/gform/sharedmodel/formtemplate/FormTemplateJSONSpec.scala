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
                                          |  }
                                          |}
      """.stripMargin)

    val summarySection = SummarySection.defaultJson(Default)

    val printSectionExpected = Json.parse(s"""
                                             |{
                                             |  "_id": "foobar",
                                             |  "originalId" : "FooBar",
                                             |  "draftRetrievalMethod": {
                                             |    "showContinueOrDeletePage": "true",
                                             |    "value": "onePerUser"
                                             |  },
                                             |  "formCategory": "default",
                                             |  "languages": [
                                             |    "en"
                                             |  ],
                                             |  "destinations": {
                                             |    "title": "Next Steps",
                                             |    "summaryPdf": "TestSummaryPdf"
                                             |  },
                                             |  "displayHMRCLogo": false,
                                             |  "parentFormSubmissionRefs": [],
                                             |  "summarySection": $summarySection
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
                                             |  "summarySection": $summarySection
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
                                 |  "summarySection": $summarySection
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
                                                                   |  "summarySection": $summarySection
                                                                   |}
                                """.stripMargin)

    val inputProgressIndicator =
      Json.parse("""
                   |{
                   |  "_id": "gform-1668",
                   |  "formName": "Add to list tttt",
                   |  "description": "Add to list",
                   |  "authConfig": {
                   |    "authModule": "anonymous"
                   |  },
                   |  "emailTemplateId": "eeitt_submission_confirmation",
                   |  "sections": [
                   |    {
                   |      "title": "Do you want to Add Owner Details",
                   |      "progressIndicator":"Name",
                   |      "progressIndicatorSize": "xl",
                   |      "fields": [
                   |        {
                   |          "id": "exampleChoice",
                   |          "type": "choice",
                   |          "label": "So you want to add another something?",
                   |          "format": "yesno"
                   |        }
                   |      ]
                   |    },
                   |    {
                   |      "id": "owner",
                   |      "type": "addToList",
                   |      "title": "This is for AddToList",
                   |      "summaryName": "owner",
                   |      "shortName": "${ownerName}",
                   |      "description": "${ownerName} ${OwnerAddress} ${exampleChoice}",
                   |      "addAnotherQuestion": {
                   |        "id": "ownerFc",
                   |        "type": "choice",
                   |        "label": "So you want to add another owner?",
                   |        "format": "yesno",
                   |        "helpText": "For children's homes, an individual provider may be more than one person."
                   |      },
                   |      "defaultPage": {
                   |        "title": "Add details",
                   |        "fields": [
                   |          {
                   |            "id": "dp1",
                   |            "label": "",
                   |            "type": "info",
                   |            "infoType": "noformat",
                   |            "infoText": "You must tell us about all directors"
                   |          }
                   |        ]
                   |      },
                   |      "limit": {
                   |        "repeatsMax": "${'3'}",
                   |        "field": {
                   |          "id": "uploadText",
                   |          "type": "info",
                   |          "label": "",
                   |          "infoText": "You can only add 3 items"
                   |        }
                   |      },
                   |      "pages": [
                   |        {
                   |          "progressIndicator":"Name",
                   |          "progressIndicatorSize": "l",
                   |          "title": "Name",
                   |          "fields": [
                   |            {
                   |              "id": "ownerName",
                   |              "label": "Enter name",
                   |              "labelSize": "xl",
                   |              "type": "text",
                   |              "format": "text"
                   |            },
                   |            {
                   |              "id": "ownerName2",
                   |              "label": "Enter name2",
                   |              "labelSize": "l",
                   |              "type": "text",
                   |              "format": "text"
                   |            }
                   |          ]
                   |        },
                   |        {
                   |          "progressIndicator": "Person ",
                   |          "title": "Amount",
                   |          "fields": [
                   |            {
                   |              "id": "Amount",
                   |              "label": "Enter Age",
                   |              "type": "text",
                   |              "format": "text"
                   |            }
                   |          ]
                   |        },
                   |        {
                   |          "progressIndicator": "Person ",
                   |          "title": "Address",
                   |          "fields": [
                   |            {
                   |              "id": "OwnerAddress",
                   |              "label": "Enter Address",
                   |              "type": "text",
                   |              "format": "text"
                   |            }
                   |          ]
                   |        }
                   |      ]
                   |    }
                   |  ],
                   |  "declarationSection": {
                   |    "shortName": "Declaration",
                   |    "title": "Declaration",
                   |    "fields": []
                   |  },
                   |  "acknowledgementSection": {
                   |    "shortName": "Acknowledgement Page",
                   |    "title": "Acknowledgement Page",
                   |    "fields": []
                   |  },
                   |  "destinations": [
                   |    {
                   |      "id": "transitionToSubmitted",
                   |      "type": "stateTransition",
                   |      "requiredState": "Submitted"
                   |    }
                   |  ]
                   |}
                   |""".stripMargin)

    val expectedProgressIndicator =
      Json.parse(
        """
          |{
          |  "draftRetrievalMethod": {
          |    "showContinueOrDeletePage": "true",
          |    "value": "onePerUser"
          |  },
          |  "formCategory": "default",
          |  "languages": [
          |    "en"
          |  ],
          |  "authConfig": {
          |    "authModule": "anonymous"
          |  },
          |  "destinations": {
          |    "destinations": [
          |      {
          |        "id": "transitionToSubmitted",
          |        "type": "stateTransition",
          |        "requiredState": "Submitted"
          |      }
          |    ],
          |    "declarationSection": {
          |      "shortName": "Declaration",
          |      "title": "Declaration",
          |      "fields": []
          |    },
          |    "acknowledgementSection": {
          |      "shortName": "Acknowledgement Page",
          |      "title": "Acknowledgement Page",
          |      "fields": [],
          |      "displayFeedbackLink": true
          |    }
          |  },
          |  "description": "Add to list",
          |  "sections": [
          |    {
          |      "title": "Do you want to Add Owner Details",
          |      "progressIndicator": {
          |        "label": "Name",
          |        "labelSize": {
          |          "ExtraLarge": {}
          |        }
          |      },
          |      "fields": [
          |        {
          |          "id": "exampleChoice",
          |          "type": "choice",
          |          "label": "So you want to add another something?",
          |          "format": "yesno"
          |        }
          |      ]
          |    },
          |    {
          |      "summaryName": "owner",
          |      "defaultPage": {
          |        "title": "Add details",
          |        "fields": [
          |          {
          |            "id": "dp1",
          |            "label": "",
          |            "type": "info",
          |            "infoType": "noformat",
          |            "infoText": "You must tell us about all directors"
          |          }
          |        ]
          |      },
          |      "pages": [
          |        {
          |          "progressIndicator": {
          |            "label": "Name",
          |            "labelSize": {
          |              "Large": {}
          |            }
          |          },
          |          "title": "Name",
          |          "fields": [
          |            {
          |              "id": "ownerName",
          |              "label": "Enter name",
          |              "labelSize": "xl",
          |              "type": "text",
          |              "format": "text"
          |            },
          |            {
          |              "id": "ownerName2",
          |              "label": "Enter name2",
          |              "labelSize": "l",
          |              "type": "text",
          |              "format": "text"
          |            }
          |          ]
          |        },
          |        {
          |          "progressIndicator": {
          |            "label": "Person ",
          |            "labelSize": {
          |              "Medium": {}
          |            }
          |          },
          |          "title": "Amount",
          |          "fields": [
          |            {
          |              "id": "Amount",
          |              "label": "Enter Age",
          |              "type": "text",
          |              "format": "text"
          |            }
          |          ]
          |        },
          |        {
          |          "progressIndicator": {
          |            "label": "Person ",
          |            "labelSize": {
          |              "Medium": {}
          |            }
          |          },
          |          "title": "Address",
          |          "fields": [
          |            {
          |              "id": "OwnerAddress",
          |              "label": "Enter Address",
          |              "type": "text",
          |              "format": "text"
          |            }
          |          ]
          |        }
          |      ],
          |      "limit": {
          |        "repeatsMax": "${'3'}",
          |        "field": {
          |          "id": "uploadText",
          |          "type": "info",
          |          "label": "",
          |          "infoText": "You can only add 3 items"
          |        }
          |      },
          |      "description": "${ownerName} ${OwnerAddress} ${exampleChoice}",
          |      "id": "owner",
          |      "type": "addToList",
          |      "title": "This is for AddToList",
          |      "shortName": "${ownerName}",
          |      "addAnotherQuestion": {
          |        "id": "ownerFc",
          |        "type": "choice",
          |        "label": "So you want to add another owner?",
          |        "format": "yesno",
          |        "helpText": "For children's homes, an individual provider may be more than one person."
          |      }
          |    }
          |  ],
          |  "summarySection": {
          |    "title": {
          |      "en": "Check your answers",
          |      "cy": "Gwiriwch eich atebion"
          |    },
          |    "header": {
          |      "en": "Make sure the information you have given is correct",
          |      "cy": "Gwnewch yn siŵr bod yr wybodaeth a roddwyd gennych yn gywir"
          |    },
          |    "footer": {
          |      "en": "##Now send your form\n\nYou need to submit your form on the next screen.\n\nBefore you do this you can [print or save a PDF copy of your answers (opens in a new window or tab)](${link.printSummaryPdf}).",
          |      "cy": "##Nawr anfonwch eich ffurflen\n\nMae angen i chi gyflwyno’ch ffurflen ar y sgrin nesaf.\n\nCyn i chi wneud hyn gallwch [argraffu neu gadw copi PDF o’ch atebion (yn agor ffenestr neu dab newydd)](${link.printSummaryPdf})."
          |    }
          |  },
          |  "formName": "Add to list tttt",
          |  "emailTemplateId": "eeitt_submission_confirmation",
          |  "_id": "gform-1668",
          |  "originalId": "gform-1668",
          |  "displayHMRCLogo": false,
          |  "parentFormSubmissionRefs": []
          |}
          |""".stripMargin
      )

    val t = Table(
      ("input", "expected"),
      (printSectionInput, printSectionExpected),
      (destinationsInput, destinationsExpected),
      (input, expected),
      (inputDraftRetrievalMethodIsNotPermitted, expectedDraftRetrievalMethodIsNotPermitted),
      (inputProgressIndicator, expectedProgressIndicator)
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
                             |  }
                             |}
      """.stripMargin)

    FormTemplatesControllerRequestHandler.normaliseJSON(input) should be(
      FormTemplatesControllerRequestHandler.onlyOneOfDestinationsAndPrintSection
    )
  }

  it should "return validation error when both destinations and printSection are missing" in {

    val input = Json.parse("""
                             |{
                             |  "_id": "TST123"
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
                             |  ]
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
                             |  }
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
                             |  }
                             |}
                           """.stripMargin)

    FormTemplatesControllerRequestHandler.normaliseJSON(input) should be(
      FormTemplatesControllerRequestHandler.avoidDeclarationForPrintSection
    )
  }

}
