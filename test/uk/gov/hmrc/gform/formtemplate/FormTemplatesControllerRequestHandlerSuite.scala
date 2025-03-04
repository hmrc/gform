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

package uk.gov.hmrc.gform.formtemplate

import munit.{ FunSuite, Location }
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{ JsDefined, JsError, JsString, JsSuccess, JsValue, Json }

class FormTemplatesControllerRequestHandlerSuite extends FunSuite with Matchers {
  test(
    "normaliseJSON should lowercased '_id' field and create new 'originalId' field holding original value of '_id' field"
  ) {

    val json = Json.parse(
      """|{
         |  "_id": "Upper-CASE-Template-Id",
         |  "formName": "Lower case _id",
         |  "description": "",
         |  "authConfig": {
         |    "authModule": "anonymous"
         |  },
         |  "emailTemplateId": "emailId",
         |  "sections": [
         |    {
         |      "title": "Page A",
         |      "fields": [
         |        {
         |          "id": "fieldA",
         |          "type": "text",
         |          "label": "Field A",
         |          "format": "shortText"
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
         |}""".stripMargin
    )

    FormTemplatesControllerRequestHandler.normaliseJSON(json) match {
      case JsSuccess(normalised, _) =>
        assertFieldValue("_id", "upper-case-template-id", normalised)
        assertFieldValue("originalId", "Upper-CASE-Template-Id", normalised)
      case JsError(error) => fail("Unable to normalise json: " + error)
    }

  }

  test(
    "normaliseJSON should transform field 'choices' of 'choice' and 'revealongChoice' components"
  ) {

    val json = Json.parse(
      """|{
         |  "_id": "choices",
         |  "sections": [
         |    {
         |      "title": "Page",
         |      "fields": [
         |        {
         |          "type": "choice",
         |          "id": "choice",
         |          "label": "Basic choice",
         |          "choices": [
         |            {
         |              "value": "foo",
         |              "en": "Yes",
         |              "cy": "Iawn"
         |            },
         |            {
         |              "en": "No",
         |              "cy": "Na"
         |            },
         |            "simple text"
         |          ]
         |        },
         |        {
         |          "id": "revealing",
         |          "type": "revealingChoice",
         |          "label": "Revealing choice",
         |          "choices": [
         |            {
         |              "value": "bar",
         |              "en": "Yes - rc",
         |              "cy": "Iawn - rc"
         |            },
         |            {
         |              "en": "en text",
         |              "cy": "cy text"
         |            },
         |            "revealing choice text"
         |          ],
         |          "revealingFields": [
         |            [
         |              {
         |                "type": "choice",
         |                "id": "nestedChoice",
         |                "label": "Nested choice",
         |                "choices": [
         |                  {
         |                    "value": "nestedFoo",
         |                    "en": "Nested Yes",
         |                    "cy": "Nested Iawn"
         |                  },
         |                  {
         |                    "en": "Nested No",
         |                    "cy": "Nested Na"
         |                  },
         |                  "Nested simple text"
         |                ]
         |              }
         |            ],
         |            [],
         |            []
         |          ]
         |        }
         |      ]
         |    }
         |  ],
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
         |}""".stripMargin
    )

    val expected =
      """|[
         |  {
         |    "fields": [
         |      {
         |        "id": "choice",
         |        "label": "Basic choice",
         |        "type": "choice",
         |        "choices": [
         |          {
         |            "value": "foo",
         |            "label": {
         |              "en": "Yes",
         |              "cy": "Iawn"
         |            }
         |          },
         |          {
         |            "label": {
         |              "en": "No",
         |              "cy": "Na"
         |            }
         |          },
         |          {
         |            "label": "simple text"
         |          }
         |        ]
         |      },
         |      {
         |        "id": "revealing",
         |        "label": "Revealing choice",
         |        "type": "revealingChoice",
         |        "choices": [
         |          {
         |            "value": "bar",
         |            "label": {
         |              "en": "Yes - rc",
         |              "cy": "Iawn - rc"
         |            }
         |          },
         |          {
         |            "label": {
         |              "en": "en text",
         |              "cy": "cy text"
         |            }
         |          },
         |          {
         |            "label": "revealing choice text"
         |          }
         |        ],
         |        "revealingFields": [
         |          [
         |            {
         |              "id": "nestedChoice",
         |              "label": "Nested choice",
         |              "type": "choice",
         |              "choices": [
         |                {
         |                  "value": "nestedFoo",
         |                  "label": {
         |                    "en": "Nested Yes",
         |                    "cy": "Nested Iawn"
         |                  }
         |                },
         |                {
         |                  "label": {
         |                    "en": "Nested No",
         |                    "cy": "Nested Na"
         |                  }
         |                },
         |                {
         |                  "label": "Nested simple text"
         |                }
         |              ]
         |            }
         |          ],
         |          [],
         |          []
         |        ]
         |      }
         |    ],
         |   "title": "Page"
         |  }
         |]""".stripMargin

    FormTemplatesControllerRequestHandler.normaliseJSON(json) match {
      case JsSuccess(normalised, _) =>
        normalised \ "formKind" \ "sections" match {
          case JsDefined(sections) => sections shouldBe Json.parse(expected)
          case otherwise           => fail(s"No sections field present in: $normalised")
        }
      case JsError(error) => fail("Unable to normalise json: " + error)
    }

  }

  test(
    "normaliseJSON should transform field 'choices' of 'choice' components for addAnotherQuestion"
  ) {

    val json = Json.parse(
      """|{
         |  "_id": "choices",
         |  "acknowledgementSection": {
         |    "shortName": "Acknowledgement Page",
         |    "title": "Acknowledgement Page",
         |    "fields": []
         |  },
         |  "sections": [
         |    {
         |      "title": "Page A",
         |      "fields": [
         |        {
         |          "id": "fieldA",
         |          "label": "Field A",
         |          "format": "text"
         |        }
         |      ]
         |    },
         |    {
         |      "type": "addToList",
         |      "title": "Add To List",
         |      "shortName": "Add To List",
         |      "summaryDescription": "${fieldA}",
         |      "description": "${fieldA}",
         |      "summaryName": "Add To List",
         |      "pages": [
         |        {
         |          "title": "Page $n",
         |          "shortName": "asdfa",
         |          "fields": [
         |            {
         |              "id": "fieldA",
         |              "type": "text",
         |              "label": "Field A",
         |              "format": "sterling"
         |            }
         |          ]
         |        }
         |      ],
         |      "addAnotherQuestion": {
         |        "id": "client",
         |        "type": "choice",
         |        "label": "label1",
         |        "format": "label1",
         |        "choices": [
         |          {
         |            "en": "Yes",
         |            "cy": "Iawn"
         |          },
         |          {
         |            "en": "No",
         |            "cy": "Na"
         |          }
         |        ]
         |      }
         |    }
         |  ],
         |  "destinations": [
         |    {
         |      "id": "transitionToSubmitted",
         |      "type": "stateTransition",
         |      "requiredState": "Submitted"
         |    }
         |  ]
         |}""".stripMargin
    )

    val expected =
      """
        | [
        | {
        |    "fields": [
        |      {
        |        "id": "fieldA",
        |        "label": "Field A",
        |        "format": "text"
        |      }
        |    ],
        |    "title": "Page A"
        |  },
        |  {
        |    "summaryName": "Add To List",
        |    "pages": [
        |      {
        |        "shortName": "asdfa",
        |        "fields": [
        |          {
        |            "id": "fieldA",
        |            "type": "text",
        |            "label": "Field A",
        |            "format": "sterling"
        |          }
        |        ],
        |        "title": "Page $n"
        |      }
        |    ],
        |    "description": "${fieldA}",
        |    "shortName": "Add To List",
        |    "type": "addToList",
        |    "title": "Add To List",
        |    "addAnotherQuestion": {
        |      "format": "label1",
        |      "id": "client",
        |      "label": "label1",
        |      "type": "choice",
        |      "choices": [
        |        {
        |          "label": {
        |            "en": "Yes",
        |            "cy": "Iawn"
        |          }
        |        },
        |        {
        |          "label": {
        |            "en": "No",
        |            "cy": "Na"
        |          }
        |        }
        |      ]
        |    },
        |    "summaryDescription": "${fieldA}"
        |  }
        |]""".stripMargin

    FormTemplatesControllerRequestHandler.normaliseJSON(json) match {
      case JsSuccess(normalised, _) =>
        normalised \ "formKind" \ "sections" match {
          case JsDefined(addAnotherQuestion) =>
            addAnotherQuestion shouldBe Json.parse(expected)
          case otherwise => fail(s"No sections field present in: $normalised")
        }

      case JsError(error) => fail("Unable to normalise json: " + error)
    }

  }

  test(
    "normaliseJSON should transform field 'choices' of 'choice' components for addAnotherQuestion when inside TaskList"
  ) {

    val json = Json.parse(
      """|{
         |  "_id": "choices",
         |  "acknowledgementSection": {
         |    "shortName": "Acknowledgement Page",
         |    "title": "Acknowledgement Page",
         |    "fields": []
         |  },
         |  "sections": [
         |    {
         |      "title": "Task List",
         |      "tasks": [
         |        {
         |          "title": "Task A details",
         |          "sections": [
         |            {
         |              "title": "Page A",
         |              "fields": [
         |                {
         |                  "id": "fieldX",
         |                  "label": "Field X",
         |                  "format": "text"
         |                }
         |              ]
         |            },
         |            {
         |              "type": "addToList",
         |              "title": "Add To List",
         |              "shortName": "Add To List",
         |              "summaryDescription": "${fieldA}",
         |              "description": "${fieldA}",
         |              "summaryName": "Add To List",
         |              "pages": [
         |                {
         |                  "title": "Page $n",
         |                  "shortName": "asdfa",
         |                  "fields": [
         |                    {
         |                      "id": "fieldA",
         |                      "type": "text",
         |                      "label": "Field A",
         |                      "format": "sterling"
         |                    }
         |                  ]
         |                }
         |              ],
         |              "addAnotherQuestion": {
         |                "id": "client",
         |                "type": "choice",
         |                "label": "label1",
         |                "format": "vertical",
         |                "choices": [
         |                  {
         |                    "en": "Yes",
         |                    "cy": "Iawn"
         |                  },
         |                  {
         |                    "en": "No",
         |                    "cy": "Na"
         |                  }
         |                ]
         |              }
         |            }
         |          ],
         |          "summarySection": {
         |            "title": "Check your answers",
         |            "header": "Make sure the information you have given is correct",
         |            "footer": ""
         |          }
         |        }
         |      ]
         |    }
         |  ],
         |  "submitSection": {
         |    "label": "Check and send to HMRC",
         |    "taskLabel": "Check answers and submit form"
         |  },
         |  "declarationSection": {
         |    "title": "Declaration",
         |    "fields": []
         |  },
         |  "destinations": [
         |    {
         |      "id": "transitionToSubmitted",
         |      "type": "stateTransition",
         |      "requiredState": "Submitted"
         |    }
         |  ]
         |}""".stripMargin
    )

    val expected =
      """
        |[
        |  {
        |    "title": "Task List",
        |    "tasks": [
        |      {
        |        "title": "Task A details",
        |        "sections": [
        |          {
        |            "title": "Page A",
        |            "fields": [
        |              {
        |                "id": "fieldX",
        |                "label": "Field X",
        |                "format": "text"
        |              }
        |            ]
        |          },
        |          {
        |            "type": "addToList",
        |            "title": "Add To List",
        |            "shortName": "Add To List",
        |            "summaryDescription": "${fieldA}",
        |            "description": "${fieldA}",
        |            "summaryName": "Add To List",
        |            "pages": [
        |              {
        |                "title": "Page $n",
        |                "shortName": "asdfa",
        |                "fields": [
        |                  {
        |                    "id": "fieldA",
        |                    "type": "text",
        |                    "label": "Field A",
        |                    "format": "sterling"
        |                  }
        |                ]
        |              }
        |            ],
        |            "addAnotherQuestion": {
        |              "id": "client",
        |              "type": "choice",
        |              "label": "label1",
        |              "format": "vertical",
        |              "choices": [
        |                {
        |                  "label": {
        |                    "en": "Yes",
        |                    "cy": "Iawn"
        |                  }
        |                },
        |                {
        |                  "label": {
        |                    "en": "No",
        |                    "cy": "Na"
        |                  }
        |                }
        |              ]
        |            }
        |          }
        |        ],
        |        "summarySection": {
        |          "title": "Check your answers",
        |          "header": "Make sure the information you have given is correct",
        |          "footer": ""
        |        }
        |      }
        |    ]
        |  }
        |]""".stripMargin

    FormTemplatesControllerRequestHandler.normaliseJSON(json) match {
      case JsSuccess(normalised, _) =>
        normalised \ "formKind" \ "sections" match {
          case JsDefined(taskListWithAddAnotherQuestion) =>
            taskListWithAddAnotherQuestion shouldBe Json.parse(expected)
          case otherwise => fail(s"No sections field present in: $normalised")
        }

      case JsError(error) => fail("Unable to normalise json: " + error)
    }
  }

  private def assertFieldValue(fieldName: String, expectedValue: String, json: JsValue)(implicit loc: Location) =
    json \ fieldName match {
      case JsDefined(JsString(str)) => assertEquals(str, expectedValue)
      case otherwise                => fail(s"$fieldName is undefined or not a string: $otherwise")
    }

}
