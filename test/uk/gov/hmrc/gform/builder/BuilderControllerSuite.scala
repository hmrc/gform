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

package uk.gov.hmrc.gform.builder

import io.circe.Json
import io.circe.literal.JsonStringContext
import io.circe.syntax._
import munit.FunSuite
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Coordinates, FormComponentId, TaskNumber, TaskSectionNumber }

class BuilderControllerSuite extends FunSuite {
  val json: Json = json"""
    {
      "sections": [
        {
          "title": "Page 1",
          "fields": [
            {
              "id": "first",
              "type": "text",
              "label": "First",
              "format": "sterling"
            }
          ]
        },
        {
          "title": "Page 2",
          "fields": [
            {
              "id": "second",
              "type": "text",
              "label": "Second",
              "format": "sterling"
            }
          ]
        }
      ]
    }"""

  val expectedJson: Json = json"""
    {
      "sections": [
        {
          "title": "Page AA",
          "fields": [
            {
              "id": "first",
              "type": "text",
              "label": "First",
              "format": "sterling"
            }
          ]
        },
        {
          "title": "Page 2",
          "fields": [
            {
              "id": "second",
              "type": "text",
              "label": "Second",
              "format": "sterling"
            }
          ]
        }
      ]
    }"""

  val expectedJson2: Json = json"""
    {
      "sections": [
        {
          "title": "Page 1",
          "fields": [
            {
              "id": "first",
              "type": "text",
              "label": "First",
              "format": "sterling"
            }
          ]
        },
        {
          "title": "Page AA",
          "fields": [
            {
              "id": "second",
              "type": "text",
              "label": "Second",
              "format": "sterling"
            }
          ]
        }
      ]
    }"""

  test("Update section title (1)") {
    val patch: Json = Json.obj("title" := "Page AA")

    val sectionPath = SectionPath(".sections[0]")

    val result = BuilderSupport.modifySectionData(json, sectionPath, patch)

    assertEquals(result, expectedJson)
  }

  test("Update section title (2)") {
    val patch: Json = Json.obj("title" := "Page AA")

    val sectionPath = SectionPath(".sections[1]")

    val result = BuilderSupport.modifySectionData(json, sectionPath, patch)

    assertEquals(result, expectedJson2)
  }

  val sectionCaptionJson: Json = json"""
    {
      "sections": [
        {
          "title": "Page 1",
          "caption": "I'm caption",
          "fields": [
            {
              "id": "first",
              "type": "text",
              "label": "First",
              "format": "sterling"
            }
          ]
        },
        {
          "title": "Page 2",
          "fields": [
            {
              "id": "second",
              "type": "text",
              "label": "Second",
              "format": "sterling"
            }
          ]
        }
      ]
    }"""

  val expectedSectionCaptionJson: Json = json"""
    {
      "sections": [
        {
          "title": "Page 1",
          "caption": "I'm caption AA",
          "fields": [
            {
              "id": "first",
              "type": "text",
              "label": "First",
              "format": "sterling"
            }
          ]
        },
        {
          "title": "Page 2",
          "fields": [
            {
              "id": "second",
              "type": "text",
              "label": "Second",
              "format": "sterling"
            }
          ]
        }
      ]
    }"""

  test("Update section caption (1)") {
    val patch: Json = Json.obj("caption" := "I'm caption AA")

    val sectionPath = SectionPath(".sections[0]")

    val result = BuilderSupport.modifySectionData(sectionCaptionJson, sectionPath, patch)

    assertEquals(result.spaces2, expectedSectionCaptionJson.spaces2)
  }

  val sectionPresentationHintJson: Json = json"""
    {
      "sections": [
        {
          "title": "Page 1",
          "caption": "I'm caption",
          "presentationHint": "invisiblePageTitle",
          "fields": [
            {
              "id": "first",
              "type": "text",
              "label": "First",
              "format": "sterling"
            }
          ]
        }
      ]
    }"""

  val expectedSectionPresentationHintJson: Json = json"""
    {
      "sections": [
        {
          "title": "Page 1",
          "caption": "I'm caption",
          "fields": [
            {
              "id": "first",
              "type": "text",
              "label": "First",
              "format": "sterling"
            }
          ]
        }
      ]
    }"""

  test("Purge section's presentationHint when empty (1)") {
    val patch: Json = Json.obj("presentationHint" := "")

    val sectionPath = SectionPath(".sections[0]")

    val result = BuilderSupport.modifySectionData(sectionPresentationHintJson, sectionPath, patch)

    assertEquals(result, expectedSectionPresentationHintJson)
  }

  val sectionPresentationHintJson2: Json = json"""
    {
      "sections": [
        {
          "title": "Page 1",
          "caption": "I'm caption",
          "fields": [
            {
              "id": "first",
              "type": "text",
              "label": "First",
              "format": "sterling"
            }
          ]
        }
      ]
    }"""

  val expectedSectionPresentationHintJson2: Json = json"""
    {
      "sections": [
        {
          "title": "Page 1",
          "caption": "I'm caption",
          "presentationHint": "abc",
          "fields": [
            {
              "id": "first",
              "type": "text",
              "label": "First",
              "format": "sterling"
            }
          ]
        }
      ]
    }"""

  test("Set section's presentationHint when empty (1)") {
    val patch: Json = Json.obj("presentationHint" := "abc")

    val sectionPath = SectionPath(".sections[0]")

    val result = BuilderSupport.modifySectionData(sectionPresentationHintJson2, sectionPath, patch)

    assertEquals(result, expectedSectionPresentationHintJson2)
  }

  val formComponentUpdateJson: Json = json"""
    {
      "sections": [
        {
          "title": "Page 1",
          "fields": [
            {
              "id": "first",
              "type": "text",
              "label": "First",
              "format": "sterling"
            }
          ]
        },
        {
          "title": "Page 2",
          "fields": [
            {
              "id": "firstName",
              "type": "text",
              "label": "First name",
              "format": "text",
              "labelSize": "s",
              "helpText": ""
            },
            {
              "id": "lastName",
              "type": "text",
              "label": "Last name",
              "format": "text",
              "labelSize": "s"
            }
          ]
        }
      ]
    }"""

  val formComponentUpdateJsonExpected: Json = json"""
    {
      "sections": [
        {
          "title": "Page 1",
          "fields": [
            {
              "id": "first",
              "type": "text",
              "label": "First",
              "format": "sterling"
            }
          ]
        },
        {
          "title": "Page 2",
          "fields": [
            {
              "id": "firstName",
              "type": "text",
              "label": "First name 22",
              "format": "text",
              "labelSize": "s",
              "helpText": ""
            },
            {
              "id": "lastName",
              "type": "text",
              "label": "Last name",
              "format": "text",
              "labelSize": "s"
            }
          ]
        }
      ]
    }"""

  val formComponentUpdateHelpTextJsonExpected: Json = json"""
    {
      "sections": [
        {
          "title": "Page 1",
          "fields": [
            {
              "id": "first",
              "type": "text",
              "label": "First",
              "format": "sterling"
            }
          ]
        },
        {
          "title": "Page 2",
          "fields": [
            {
              "id": "firstName",
              "type": "text",
              "label": "First name",
              "format": "text",
              "labelSize": "s",
              "helpText": "I'm help text"
            },
            {
              "id": "lastName",
              "type": "text",
              "label": "Last name",
              "format": "text",
              "labelSize": "s"
            }
          ]
        }
      ]
    }"""

  test("Update field label (1)") {
    val formComponentId = FormComponentId("firstName")

    val patch: Json = Json.obj("label" := "First name 22")

    val result: Json = BuilderSupport.modifyFormComponentData(formComponentUpdateJson, formComponentId, patch)

    assertEquals(result, formComponentUpdateJsonExpected)
  }

  test("Update field helpText (1)") {
    val formComponentId = FormComponentId("firstName")

    val patch: Json = Json.obj("helpText" := "I'm help text")

    val result: Json = BuilderSupport.modifyFormComponentData(formComponentUpdateJson, formComponentId, patch)

    assertEquals(result, formComponentUpdateHelpTextJsonExpected)
  }

  test("Update field shortName - set") {

    val input: Json = json"""
    {
      "sections": [
        {
          "fields": [
            {
              "id": "first",
              "type": "text",
              "label": "First",
              "format": "sterling"
            },
            {
              "id": "second",
              "type": "text",
              "label": "Second",
              "format": "sterling"
            }
          ]
        }
      ]
    }"""

    val expected: Json = json"""
    {
      "sections": [
        {
          "fields": [
            {
              "id": "first",
              "type": "text",
              "label": "First",
              "format": "sterling"
            },
            {
              "id": "second",
              "type": "text",
              "label": "Second",
              "format": "sterling",
              "shortName": "I'm shortName"
            }
          ]
        }
      ]
    }"""

    val formComponentId = FormComponentId("second")

    val patch: Json = Json.obj("shortName" := "I'm shortName")

    val result: Json = BuilderSupport.modifyFormComponentData(input, formComponentId, patch)

    assertEquals(result, expected)
  }

  test("Update field shortName - delete") {

    val input: Json = json"""
    {
      "sections": [
        {
          "fields": [
            {
              "id": "first",
              "type": "text",
              "label": "First",
              "format": "sterling"
            },
            {
              "id": "second",
              "type": "text",
              "label": "Second",
              "format": "sterling",
              "shortName": "I'm shortName"
            }
          ]
        }
      ]
    }"""

    val expected: Json = json"""
    {
      "sections": [
        {
          "fields": [
            {
              "id": "first",
              "type": "text",
              "label": "First",
              "format": "sterling"
            },
            {
              "id": "second",
              "type": "text",
              "label": "Second",
              "format": "sterling"
            }
          ]
        }
      ]
    }"""

    val formComponentId = FormComponentId("second")

    val patch: Json = Json.obj("shortName" := "")

    val result: Json = BuilderSupport.modifyFormComponentData(input, formComponentId, patch)

    assertEquals(result, expected)
  }

  val atlJson = json"""
    {
      "sections": [
        {
          "title": "What is your non-UK address?",
          "fields": [
            {
              "id": "overseasAddress",
              "type": "overseasAddress",
              "shortName": "Non-UK address",
              "label": "What is your non-UK address?",
              "helpText": "overseas address help",
              "mandatory": "false"
            }
          ]
        },
        {
          "title": "Page A title",
          "fields": [
            {
              "id": "second",
              "type": "text",
              "label": "Component A label",
              "format": "sterling"
            }
          ]
        },
        {
          "type": "addToList",
          "title": "Notifications you have added",
          "addAnotherQuestion": {
            "type": "choice",
            "label": "Do you want to add another notification?",
            "id": "page1",
            "format": "yesno",
            "errorMessage": "Select yes if you want to add another notification"
          },
          "pages": [
            {
              "title": "What type of notification are you telling HMRC about?",
              "caption": "Trade mark",
              "shortName": "What type of notification",
              "fields": [
                {
                  "type": "choice",
                  "id": "whatNotification2",
                  "label": "XX What type of notification are you telling HMRC about?",
                  "errorMessage": "Select what type of notification you are telling us about",
                  "shortName": "Notification type",
                  "mandatory": "yes",
                  "choices": [
                    "New ramping up period for an existing QAHC regime",
                    "Company does not expect to meet the ownership condition",
                    "Exit the regime after breaching a condition"
                  ],
                  "hints": [
                    "This is for companies which previously met all entry conditions",
                    "This is during an existing ramping up period",
                    "Also known as 'breach and exit'"
                  ]
                }
              ]
            },
            {
              "title": "Who are the contacts?",
              "caption": "test",
              "fields": [
                {
                  "id": "agentFullName",
                  "type": "text",
                  "format": "shortText",
                  "label": "Name of agent",
                  "shortName": "Agent name"
                },
                {
                  "id": "whatNotification",
                  "type": "text",
                  "format": "shortText",
                  "label": "Name of owner",
                  "shortName": "Owner name"
                },
                {
                  "id": "agentFullName2",
                  "type": "text",
                  "format": "shortText",
                  "label": "Name of owner",
                  "shortName": "Owner name"
                }
              ]
            }
          ]
        }
      ]
    }"""

  val expectedAtlJson = json"""
    {
      "sections": [
        {
          "title": "What is your non-UK address?",
          "fields": [
            {
              "id": "overseasAddress",
              "type": "overseasAddress",
              "shortName": "Non-UK address",
              "label": "What is your non-UK address?",
              "helpText": "overseas address help",
              "mandatory": "false"
            }
          ]
        },
        {
          "title": "Page A title",
          "fields": [
            {
              "id": "second",
              "type": "text",
              "label": "Component A label",
              "format": "sterling"
            }
          ]
        },
        {
          "type": "addToList",
          "title": "Notifications you have added",
          "addAnotherQuestion": {
            "type": "choice",
            "label": "Do you want to add another notification?",
            "id": "page1",
            "format": "yesno",
            "errorMessage": "Select yes if you want to add another notification"
          },
          "pages": [
            {
              "title": "What type of notification are you telling HMRC about?",
              "caption": "Trade mark",
              "shortName": "What type of notification",
              "fields": [
                {
                  "type": "choice",
                  "id": "whatNotification2",
                  "label": "XX What type of notification are you telling HMRC about?",
                  "errorMessage": "Select what type of notification you are telling us about",
                  "shortName": "Notification type",
                  "mandatory": "yes",
                  "choices": [
                    "New ramping up period for an existing QAHC regime",
                    "Company does not expect to meet the ownership condition",
                    "Exit the regime after breaching a condition"
                  ],
                  "hints": [
                    "This is for companies which previously met all entry conditions",
                    "This is during an existing ramping up period",
                    "Also known as 'breach and exit'"
                  ]
                }
              ]
            },
            {
              "title": "Who are the contacts?",
              "caption": "test",
              "fields": [
                {
                  "id": "agentFullName",
                  "type": "text",
                  "format": "shortText",
                  "label": "Name of agent",
                  "shortName": "Agent name"
                },
                {
                  "helpText": "Help text 22",
                  "id": "whatNotification",
                  "type": "text",
                  "format": "shortText",
                  "label": "First name 22",
                  "shortName": "Owner name"
                },
                {
                  "id": "agentFullName2",
                  "type": "text",
                  "format": "shortText",
                  "label": "Name of owner",
                  "shortName": "Owner name"
                }
              ]
            }
          ]
        }
      ]
    }"""

  test("Update field label in addToList") {
    val formComponentId = FormComponentId("whatNotification")

    val patch: Json = Json.obj("label" := "First name 22", "helpText" := "Help text 22")

    val result: Json = BuilderSupport.modifyFormComponentData(atlJson, formComponentId, patch)

    assertEquals(result.spaces2, expectedAtlJson.spaces2)
  }

  val taskListJson = json"""
    {
      "sections": [
        {
          "title": "Check before you start",
          "tasks": [
            {
              "title": "Check eligibility",
              "sections": [
                {
                  "title": "Applicants age details page 1",
                  "fields": [
                    {
                      "id": "foo",
                      "type": "text",
                      "notPII": "true",
                      "format": "text",
                      "label": "Foo"
                    },
                    {
                      "id": "bar",
                      "type": "text",
                      "notPII": "true",
                      "format": "text",
                      "label": "Bar"
                    }
                  ]
                }
              ]
            }
          ]
        },
        {
          "title": "Apply",
          "tasks": [
            {
              "title": "Submit and pay",
              "sections": [
                {
                  "title": "Page",
                  "fields": [
                    {
                      "id": "name",
                      "type": "text",
                      "format": "text",
                      "label": "Name"
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    }"""

  val expectedTaskListJson = json"""
    {
      "sections": [
        {
          "title": "Check before you start",
          "tasks": [
            {
              "title": "Check eligibility",
              "sections": [
                {
                  "title": "Applicants age details page 1",
                  "fields": [
                    {
                      "id": "foo",
                      "type": "text",
                      "notPII": "true",
                      "format": "text",
                      "label": "Foo"
                    },
                    {
                      "id": "bar",
                      "type": "text",
                      "notPII": "true",
                      "format": "text",
                      "label": "Bar 2"
                    }
                  ]
                }
              ]
            }
          ]
        },
        {
          "title": "Apply",
          "tasks": [
            {
              "title": "Submit and pay",
              "sections": [
                {
                  "title": "Page",
                  "fields": [
                    {
                      "id": "name",
                      "type": "text",
                      "format": "text",
                      "label": "Name"
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    }"""

  test("Update field label in tasklist") {
    val formComponentId = FormComponentId("bar")

    val patch: Json = Json.obj("label" := "Bar 2")

    val result: Json = BuilderSupport.modifyFormComponentData(taskListJson, formComponentId, patch)

    assertEquals(result.spaces2, expectedTaskListJson.spaces2)
  }

  val taskListWithAtlJson = json"""
    {
      "sections": [
        {
          "title": "Check before you start",
          "tasks": [
            {
              "title": "Check eligibility",
              "sections": [
                {
                  "title": "Applicants age details",
                  "fields": [
                    {
                      "id": "foo",
                      "type": "text",
                      "notPII": "true",
                      "format": "text",
                      "label": "Foo"
                    }
                  ]
                }
              ]
            }
          ]
        },
        {
          "title": "Prepare pet application",
          "tasks": [
            {
              "title": "Pet information",
              "sections": [
                {
                  "type": "addToList",
                  "pages": [
                    {
                      "title": "Pet details",
                      "fields": [
                        {
                          "id": "petName",
                          "label": "Pet name",
                          "type": "text",
                          "format": "text"
                        }
                      ]
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    }"""

  val expectedTaskListWithAtlJson = json"""
    {
      "sections": [
        {
          "title": "Check before you start",
          "tasks": [
            {
              "title": "Check eligibility",
              "sections": [
                {
                  "title": "Applicants age details",
                  "fields": [
                    {
                      "id": "foo",
                      "type": "text",
                      "notPII": "true",
                      "format": "text",
                      "label": "Foo"
                    }
                  ]
                }
              ]
            }
          ]
        },
        {
          "title": "Prepare pet application",
          "tasks": [
            {
              "title": "Pet information",
              "sections": [
                {
                  "type": "addToList",
                  "pages": [
                    {
                      "title": "Pet details",
                      "fields": [
                        {
                          "id": "petName",
                          "label": "Pet name 2",
                          "type": "text",
                          "format": "text"
                        }
                      ]
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    }"""

  test("Update field label in atl in tasklist") {
    val formComponentId = FormComponentId("petName")

    val patch: Json = Json.obj("label" := "Pet name 2")

    val result: Json = BuilderSupport.modifyFormComponentData(taskListWithAtlJson, formComponentId, patch)

    assertEquals(result.spaces2, expectedTaskListWithAtlJson.spaces2)
  }

  val summarySectionJson: Json = json"""
    {
      "summarySection": {
        "title": "This is 'title'",
        "header": "#This is markdown *'header'*",
        "footer": "#This is markdowm *'footer'*",
        "displayWidth": "l",
        "continueLabel": "Confirm"
      }
    }"""

  val summarySectionJsonExpected: Json = json"""
    {
      "summarySection": {
        "title": "Foo",
        "header": "#This is markdown *'header'*",
        "footer": "#This is markdowm *'footer'*",
        "displayWidth": "l",
        "continueLabel": "Confirm"
      }
    }"""

  test("Update summary section title") {

    val patch: Json = Json.obj("title" := "Foo")

    val result: Json = BuilderSupport.modifySummarySectionData(summarySectionJson, patch, None)

    assertEquals(result, summarySectionJsonExpected)
  }

  val noSummarySectionJson: Json = json"""
    {
      "formName": "Default summary section"
    }"""

  val noSummarySectionJsonExpected: Json = json"""
    {
      "formName": "Default summary section",
      "summarySection": {
        "title": "Foo",
        "header": {
          "en": "Make sure the information you have given is correct.",
          "cy": "Gwnewch yn siŵr bod yr wybodaeth a roddwyd gennych yn gywir."
        },
        "footer": {
          "en": "##Now send your form\n\nYou need to submit your form on the next screen.\n\nBefore you do this you can [print or save a PDF copy of your answers (opens in a new window or tab)]($${link.printSummaryPdf}).",
          "cy": "##Nawr anfonwch eich ffurflen\n\nMae angen i chi gyflwyno’ch ffurflen ar y sgrin nesaf.\n\nCyn i chi wneud hyn gallwch [argraffu neu gadw copi PDF o’ch atebion (yn agor ffenestr neu dab newydd)]($${link.printSummaryPdf})."
        }
      }
    }"""

  test("Update summary section title (when summarySection is missing)") {

    val patch: Json = Json.obj("title" := "Foo")

    val result: Json = BuilderSupport.modifySummarySectionData(noSummarySectionJson, patch, None)

    assertEquals(result, noSummarySectionJsonExpected)
  }

  val noSummarySectionWithFormCategoryJson: Json = json"""
    {
      "formName": "Default summary section",
      "formCategory" : "hmrcReturnForm"
    }"""

  val noSummarySectionWithFormCategoryJsonExpected: Json = json"""
    {
      "formName": "Default summary section",
      "formCategory": "hmrcReturnForm",
      "summarySection": {
        "title": "Foo",
        "header": {
          "en": "Make sure the information you have given is correct.",
          "cy": "Gwnewch yn siŵr bod yr wybodaeth a roddwyd gennych yn gywir."
        },
        "footer": {
          "en": "##Now send your return\n\nYou need to submit your return on the next screen.\n\nBefore you do this you can [print or save a PDF copy of your answers (opens in a new window or tab)]($${link.printSummaryPdf}).",
          "cy": "##Nawr anfonwch eich datganiad\n\nMae angen i chi gyflwyno’ch datganiad ar y sgrin nesaf.\n\nCyn i chi wneud hyn gallwch [argraffu neu gadw copi PDF o’ch atebion (yn agor ffenestr neu dab newydd)]($${link.printSummaryPdf})."
        }
      }
    }"""

  test("Update summary section title (when summarySection is missing and formCategory is set)") {

    val patch: Json = Json.obj("title" := "Foo")

    val result: Json = BuilderSupport.modifySummarySectionData(noSummarySectionWithFormCategoryJson, patch, None)

    assertEquals(result, noSummarySectionWithFormCategoryJsonExpected)
  }

  val summarySectionFieldsJson: Json = json"""
    {
      "summarySection": {
        "title": "This is 'title'",
        "header": "#This is markdown *'header'*",
        "footer": "#This is markdowm *'footer'*",
        "fields": [
          {
            "type": "info",
            "id": "summaryDeclaration",
            "label": "Acknowledgement copy",
            "infoText": "thingy copy"
          }
        ]
      }
    }"""

  val summarySectionFieldsJsonExpected: Json = json"""
    {
      "summarySection": {
        "title": "This is 'title'",
        "header": "#This is markdown *'header'*",
        "footer": "#This is markdowm *'footer'*",
        "fields": [
          {
            "type": "info",
            "id": "summaryDeclaration",
            "label": "Acknowledgement copy",
            "infoText": "thingy copy bar"
          }
        ]
      }
    }"""

  test("Update summary section field") {

    val formComponentId = FormComponentId("summaryDeclaration")

    val patch: Json = Json.obj("infoText" := "thingy copy bar")

    val result: Json =
      BuilderSupport.modifySummarySectionFormComponentData(summarySectionFieldsJson, formComponentId, patch, None)

    assertEquals(result, summarySectionFieldsJsonExpected)
  }

  val summarySectionTaskJson: Json = json"""
    {
      "sections": [
        {
          "tasks": [
            {
              "summarySection": {
                "title": "title",
                "header": "header",
                "footer": "footer"
              }
            }
          ]
        }
      ]
    }"""

  val summarySectionTaskJsonExpected: Json = json"""
    {
      "sections": [
        {
          "tasks": [
            {
              "summarySection": {
                "title": "Foo",
                "header": "header",
                "footer": "footer"
              }
            }
          ]
        }
      ]
    }"""

  test("Update summary section title of a task") {

    val patch: Json = Json.obj("title" := "Foo")

    val result: Json = BuilderSupport.modifySummarySectionData(
      summarySectionTaskJson,
      patch,
      Some(Coordinates(TaskSectionNumber(0), TaskNumber(0)))
    )

    assertEquals(result, summarySectionTaskJsonExpected)
  }

  val summarySectionTaskFieldsJson: Json = json"""
    {
      "sections": [
        {
          "tasks": [
            {
              "summarySection": {
                "title": "title",
                "header": "header",
                "footer": "footer",
                "fields": [
                  {
                    "type": "info",
                    "id": "summaryDeclaration",
                    "label": "Acknowledgement copy",
                    "infoText": "thingy copy"
                  }
                ]
              }
            }
          ]
        }
      ]
    }"""

  val summarySectionTaskFieldsJsonExpected: Json = json"""
    {
      "sections": [
        {
          "tasks": [
            {
              "summarySection": {
                "title": "title",
                "header": "header",
                "footer": "footer",
                "fields": [
                  {
                    "type": "info",
                    "id": "summaryDeclaration",
                    "label": "Acknowledgement copy",
                    "infoText": "thingy copy bar"
                  }
                ]
              }
            }
          ]
        }
      ]
    }"""

  test("Update summary section field of a task") {

    val formComponentId = FormComponentId("summaryDeclaration")

    val patch: Json = Json.obj("infoText" := "thingy copy bar")

    val result: Json =
      BuilderSupport.modifySummarySectionFormComponentData(
        summarySectionTaskFieldsJson,
        formComponentId,
        patch,
        Some(Coordinates(TaskSectionNumber(0), TaskNumber(0)))
      )

    assertEquals(result, summarySectionTaskFieldsJsonExpected)
  }

  val atlRepeaterJson: Json = json"""
    {
      "sections": [
        {
          "type": "addToList",
          "title": "Add To List",
          "addAnotherQuestion": {
            "id": "addToList",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "pages": []
        },
        {
          "type": "addToList",
          "title": "Add To List",
          "addAnotherQuestion": {
            "id": "addToList2",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "pages": []
        }
      ]
    }"""

  val atlRepeaterJsonExpected: Json = json"""
    {
      "sections": [
        {
          "type": "addToList",
          "title": "Add To List",
          "addAnotherQuestion": {
            "id": "addToList",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "pages": []
        },
        {
          "type": "addToList",
          "title": "Foo",
          "addAnotherQuestion": {
            "id": "addToList2",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "pages": []
        }
      ]
    }"""

  test("Update addToList title") {

    val sectionPath = SectionPath(".sections[1]")

    val patch: Json = Json.obj("title" := "Foo")

    val result: Json = BuilderSupport.modifyAtlRepeaterData(atlRepeaterJson, patch, sectionPath)

    assertEquals(result.spaces2, atlRepeaterJsonExpected.spaces2)
  }

  val atlRepeaterAddAnotherQuestionJson: Json = json"""
    {
      "sections": [
        {
          "type": "addToList",
          "title": "Add To List",
          "addAnotherQuestion": {
            "id": "addToList",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "pages": []
        },
        {
          "type": "addToList",
          "title": "Add To List",
          "addAnotherQuestion": {
            "id": "addToList2",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "pages": []
        }
      ]
    }"""

  val atlRepeaterAddAnotherQuestionJsonExpected: Json = json"""
    {
      "sections": [
        {
          "type": "addToList",
          "title": "Add To List",
          "addAnotherQuestion": {
            "id": "addToList",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "pages": []
        },
        {
          "type": "addToList",
          "title": "Add To List",
          "addAnotherQuestion": {
            "errorMessage": "This is error",
            "id": "addToList2",
            "type": "choice",
            "label": "Foo",
            "format": "yesno"
          },
          "pages": []
        }
      ]
    }"""

  test("Update addToList addAnotherQuestion label") {

    val sectionPath = SectionPath(".sections[1]")

    val patch: Json = Json.obj("label" := "Foo", "errorMessage" := "This is error")

    val result: Json =
      BuilderSupport.modifyAtlRepeaterDataAddAnotherQuestion(
        atlRepeaterAddAnotherQuestionJson,
        patch,
        sectionPath
      )

    assertEquals(result.spaces2, atlRepeaterAddAnotherQuestionJsonExpected.spaces2)
  }

  val atlRepeaterInfoFieldJson: Json = json"""
    {
      "sections": [
        {
          "type": "addToList",
          "title": "Add To List",
          "addAnotherQuestion": {
            "id": "addToList",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "pages": []
        },
        {
          "type": "addToList",
          "title": "Add To List",
          "addAnotherQuestion": {
            "id": "addToList2",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "fields": [
            {
              "id": "info1",
              "type": "info",
              "label": "",
              "infoType": "noformat",
              "infoText": "This is info field"
            }
          ],
          "pages": []
        }
      ]
    }"""

  val atlRepeaterInfoFieldJsonExpected: Json = json"""
    {
      "sections": [
        {
          "type": "addToList",
          "title": "Add To List",
          "addAnotherQuestion": {
            "id": "addToList",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "pages": []
        },
        {
          "type": "addToList",
          "title": "Add To List",
          "addAnotherQuestion": {
            "id": "addToList2",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "fields": [
            {
              "id": "info1",
              "type": "info",
              "label": "",
              "infoType": "noformat",
              "infoText": "Foo"
            }
          ],
          "pages": []
        }
      ]
    }"""

  test("Update addToList info field") {

    val sectionPath = SectionPath(".sections[1]")
    val formComponentId = FormComponentId("info1")

    val patch: Json = Json.obj("infoText" := "Foo")

    val result: Json =
      BuilderSupport.modifyAtlRepeaterFormComponentData(
        atlRepeaterInfoFieldJson,
        patch,
        formComponentId,
        sectionPath
      )

    assertEquals(result.spaces2, atlRepeaterInfoFieldJsonExpected.spaces2)
  }

  val formTemplateJson: Json = json"""
    {
      "_id": "example"
    }"""

  val formTemplateExpectedJson: Json = json"""
    {
      "displayWidth": "xl",
      "submitSection": {
        "label": "Apply",
        "taskLabel": "Submit"
      },
      "_id": "example"
    }"""

  test("Update formTemplate submitSection and displayWidth") {

    val patch: Json =
      Json.obj("submitSection" := Json.obj("label" := "Apply", "taskLabel" := "Submit"), "displayWidth" := "xl")

    val result: Json =
      BuilderSupport.modifyFormTemplate(formTemplateJson, patch)

    assertEquals(result.spaces2, formTemplateExpectedJson.spaces2)
  }

  val atlDefaultPageJson: Json = json"""
    {
      "sections": [
        {
          "type": "addToList",
          "title": "Add To List",
          "defaultPage": {
            "title": "Give us details of each cat"
          },
          "addAnotherQuestion": {
            "id": "addToList",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "pages": []
        },
        {
          "type": "addToList",
          "title": "Add To List",
          "defaultPage": {
            "title": "Give us details of each dog"
          },
          "addAnotherQuestion": {
            "id": "addToList2",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "pages": []
        }
      ]
  }"""

  val atlDefaultPageJsonExpected: Json = json"""
    {
      "sections": [
        {
          "type": "addToList",
          "title": "Add To List",
          "defaultPage": {
            "title": "Give us details of each cat"
          },
          "addAnotherQuestion": {
            "id": "addToList",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "pages": []
        },
        {
          "type": "addToList",
          "title": "Add To List",
          "defaultPage": {
            "note": "Welcome",
            "title": "Foo"
          },
          "addAnotherQuestion": {
            "id": "addToList2",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "pages": []
        }
      ]
    }"""

  test("Update addToList defaultPage title") {

    val sectionPath = SectionPath(".sections[1]")

    val patch: Json = Json.obj("title" := "Foo", "note" := "Welcome")

    val result: Json =
      BuilderSupport.modifyAtlDefaultPageData(
        atlDefaultPageJson,
        patch,
        sectionPath
      )

    assertEquals(result.spaces2, atlDefaultPageJsonExpected.spaces2)
  }

  val atlDefaultPageInfoFieldJson: Json = json"""
    {
      "sections": [
        {
          "type": "addToList",
          "title": "Add To List",
          "addAnotherQuestion": {
            "id": "addToList",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "pages": []
        },
        {
          "type": "addToList",
          "title": "Add To List",
          "defaultPage": {
            "title": "Foo",
            "fields": [
              {
                "id": "info1",
                "type": "info",
                "label": "",
                "infoType": "noformat",
                "infoText": "This is info field"
              }
            ]
          },
          "addAnotherQuestion": {
            "id": "addToList2",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "fields": [],
          "pages": []
        }
      ]
  }"""

  val atlDefaultPageInfoFieldJsonExpected: Json = json"""
    {
      "sections": [
        {
          "type": "addToList",
          "title": "Add To List",
          "addAnotherQuestion": {
            "id": "addToList",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "pages": []
        },
        {
          "type": "addToList",
          "title": "Add To List",
          "defaultPage": {
            "title": "Foo",
            "fields": [
              {
                "id": "info1",
                "type": "info",
                "label": "",
                "infoType": "noformat",
                "infoText": "Foo"
              }
            ]
          },
          "addAnotherQuestion": {
            "id": "addToList2",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "fields": [],
          "pages": []
        }
      ]
    }"""

  test("Update addToList defaultPage info field") {
    val sectionPath = SectionPath(".sections[1]")
    val formComponentId = FormComponentId("info1")

    val patch: Json = Json.obj("infoText" := "Foo")

    val result: Json =
      BuilderSupport.modifyAtlDefaultPageFormComponentData(
        atlDefaultPageInfoFieldJson,
        patch,
        formComponentId,
        sectionPath
      )

    assertEquals(result.spaces2, atlDefaultPageInfoFieldJsonExpected.spaces2)
  }

  val atlCyaPageJson: Json = json"""
    {
      "sections": [
        {
          "type": "addToList",
          "title": "Add To List",
          "cyaPage": {
            "title": "Give us details of each cat"
          },
          "addAnotherQuestion": {
            "id": "addToList",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "pages": []
        },
        {
          "type": "addToList",
          "title": "Add To List",
          "cyaPage": {
            "title": "Give us details of each dog"
          },
          "addAnotherQuestion": {
            "id": "addToList2",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "pages": []
        }
      ]
  }"""

  val atlCyaPageJsonExpected: Json = json"""
    {
      "sections": [
        {
          "type": "addToList",
          "title": "Add To List",
          "cyaPage": {
            "title": "Give us details of each cat"
          },
          "addAnotherQuestion": {
            "id": "addToList",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "pages": []
        },
        {
          "type": "addToList",
          "title": "Add To List",
          "cyaPage": {
            "note": "Welcome",
            "title": "Foo"
          },
          "addAnotherQuestion": {
            "id": "addToList2",
            "type": "choice",
            "label": "Add another?",
            "format": "yesno"
          },
          "pages": []
        }
      ]
    }"""

  test("Update addToList cyaPage title") {

    val sectionPath = SectionPath(".sections[1]")

    val patch: Json = Json.obj("title" := "Foo", "note" := "Welcome")

    val result: Json =
      BuilderSupport.modifyAtlCyaPageData(
        atlCyaPageJson,
        patch,
        sectionPath
      )

    assertEquals(result.spaces2, atlCyaPageJsonExpected.spaces2)
  }

  val localisedOptions = json"""
    {
      "id": "reasonForQuery",
      "type": "choice",
      "label": {
        "en": "What do you need help about?",
        "cy": "Am beth mae angen help arnoch chi?"
      },
      "errorMessage": {
        "en": "Wrong reason",
        "cy": "Rheswm anghywir"
      },
      "choices": [
        {
          "en": "Complex international issues",
          "cy": "Materion rhyngwladol cymhleth"
        },
        {
          "en": "Sudden increase in wealth",
          "cy": "Cynnydd sydyn mewn cyfoeth"
        },
        {
          "en": "Other complex or ambiguous tax compliance issues",
          "cy": "Materion cydymffurfio treth cymhleth neu amwys eraill"
        }
      ]
    }"""

  test("Do not update choice options") {

    val patch: Json = Json.obj(
      "id" := "reasonForQuery",
      "label" := "What do you need help about?",
      "mandatory" := "",
      "choices" := Json.arr(
        "Complex international issues".asJson,
        "Sudden increase in wealth".asJson,
        "Other complex or ambiguous tax compliance issues".asJson
      )
    )

    val result: Json =
      BuilderSupport
        .updateFormComponent(
          localisedOptions,
          patch
        )

    assertEquals(result.spaces2, localisedOptions.spaces2)
  }

  val updateChoicesExpectedJson = json"""
    {
      "id": "reasonForQuery",
      "type": "choice",
      "label": {
        "en": "What do you need help about?",
        "cy": "Am beth mae angen help arnoch chi?"
      },
      "errorMessage": {
        "en": "Wrong reason",
        "cy": "Rheswm anghywir"
      },
      "choices": [
        "Yes",
        "No"
      ]
    }
    """

  test("Update choice options") {

    val patch: Json = Json.obj(
      "id" := "reasonForQuery",
      "label" := "What do you need help about?",
      "mandatory" := "",
      "choices" := Json.arr(
        "Yes".asJson,
        "No".asJson
      )
    )

    val result: Json =
      BuilderSupport
        .updateFormComponent(
          localisedOptions,
          patch
        )

    assertEquals(result.spaces2, updateChoicesExpectedJson.spaces2)
  }

  val nonLocalisedOptions = json"""
    {
      "format" : "yesno",
      "id" : "reasonForQuery",
      "type" : "choice",
      "label" : {
        "en" : "What do you need help about?",
        "cy" : "Am beth mae angen help arnoch chi?"
      },
      "shortName" : {
        "en" : "What do you need?",
        "cy" : "Beth sydd ei angen arnoch chi?"
      },
      "helpText" : {
        "en" : "What do you need to make pancakes?",
        "cy" : "Beth sydd ei angen arnoch i wneud crempogau?"
      },
      "errorMessage" : {
        "en" : "Wrong reason",
        "cy" : "Rheswm anghywir"
      },
      "dividerText" : {
        "en" : "Or",
        "cy" : "Neu"
      }
    }"""

  val nonLocalisedOptionsExpectedJson = json"""
    {
      "multivalue": "true",
      "choices": [
        "Complex international issues",
        "Sudden increase in wealth",
        "Other complex or ambiguous tax compliance issues"
      ],
      "id" : "reasonForQuery",
      "type" : "choice",
      "label" : {
        "en" : "What do you need help about?",
        "cy" : "Am beth mae angen help arnoch chi?"
      },
      "shortName" : {
        "en" : "What do you need?",
        "cy" : "Beth sydd ei angen arnoch chi?"
      },
      "helpText" : {
        "en" : "What do you need to make pancakes?",
        "cy" : "Beth sydd ei angen arnoch i wneud crempogau?"
      },
      "errorMessage" : {
        "en" : "Wrong reason",
        "cy" : "Rheswm anghywir"
      },
      "dividerText" : {
        "en" : "Or",
        "cy" : "Neu"
      }
    }"""

  test("Do not update choice non localised options") {
    val patch: Json = Json.obj(
      "id" := "reasonForQuery",
      "label" := "What do you need help about?",
      "helpText" := "What do you need to make pancakes?",
      "shortName" := "What do you need?",
      "mandatory" := "",
      "choices" := Json.arr(
        "Complex international issues".asJson,
        "Sudden increase in wealth".asJson,
        "Other complex or ambiguous tax compliance issues".asJson
      ),
      "format" := "",
      "multivalue" := "true",
      "errorMessage" := "Wrong reason",
      "dividerPosition" := "",
      "dividerText" := "Or",
      "noneChoice" := "",
      "noneChoiceError" := ""
    )

    val result: Json =
      BuilderSupport
        .updateFormComponent(
          nonLocalisedOptions,
          patch
        )

    assertEquals(result.spaces2, nonLocalisedOptionsExpectedJson.spaces2)
  }
}
