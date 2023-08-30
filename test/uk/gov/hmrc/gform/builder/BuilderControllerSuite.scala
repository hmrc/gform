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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

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

    val result = BuilderSupport.modifySectionData(json, ".sections[0]", patch)

    assertEquals(result, expectedJson)
  }

  test("Update section title (2)") {
    val patch: Json = Json.obj("title" := "Page AA")

    val result = BuilderSupport.modifySectionData(json, ".sections[1]", patch)

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

    val result = BuilderSupport.modifySectionData(sectionCaptionJson, ".sections[0]", patch)

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

    val result = BuilderSupport.modifySectionData(sectionPresentationHintJson, ".sections[0]", patch)

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

    val result = BuilderSupport.modifySectionData(sectionPresentationHintJson2, ".sections[0]", patch)

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

}
