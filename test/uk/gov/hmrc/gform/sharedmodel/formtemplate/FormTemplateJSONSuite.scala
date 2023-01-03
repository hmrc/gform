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

import munit.FunSuite
import play.api.libs.json.{ JsObject, JsSuccess, Json }
import uk.gov.hmrc.gform.formtemplate.FormTemplatesControllerRequestHandler

class FormTemplateJSONSuite extends FunSuite {

  test("annotate sections with kind: classic") {

    val formKindClassicInput =
      Json.parse(
        """|{
           |  "_id": "foo",
           |  "formName": "Classic",
           |  "description": "",
           |  "emailTemplateId": "eeitt_submission_confirmation",
           |  "authConfig": {
           |    "authModule": "anonymous"
           |  },
           |  "sections": [
           |    {
           |      "title": "Start date",
           |      "fields": [
           |        {
           |          "id": "startDate",
           |          "type": "date",
           |          "label": "Start Date1"
           |        }
           |      ]
           |    }
           |  ],
           |  "acknowledgementSection": {
           |    "title": "Confirmation page ",
           |    "fields": []
           |  },
           |  "summarySection": {
           |    "title": "Check your answers"
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

    val expectedAnnotatedClassic =
      Json
        .parse(
          """|{
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
             |    "acknowledgementSection": {
             |      "title": "Confirmation page ",
             |      "fields": [],
             |      "displayFeedbackLink": true
             |    }
             |  },
             |  "description": "",
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
             |    "sections": [
             |      {
             |        "title": "Start date",
             |        "fields": [
             |          {
             |            "id": "startDate",
             |            "type": "date",
             |            "label": "Start Date1"
             |          }
             |        ]
             |      }
             |    ]
             |  },
             |  "summarySection": {
             |    "title": "Check your answers"
             |  },
             |  "formName": "Classic",
             |  "emailTemplateId": "eeitt_submission_confirmation",
             |  "_id": "foo",
             |  "originalId": "foo",
             |  "displayHMRCLogo": false,
             |  "parentFormSubmissionRefs": []
             |}""".stripMargin
        )
        .as[JsObject]

    assertEquals(
      FormTemplatesControllerRequestHandler.normaliseJSON(formKindClassicInput).map(Json.prettyPrint),
      JsSuccess(Json.prettyPrint(expectedAnnotatedClassic))
    )
  }

  test("annotate sections with kind: taskList") {

    val formKindTaskListInput =
      Json.parse(
        """|{
           |  "_id": "foo",
           |  "formName": "Task List",
           |  "description": "",
           |  "emailTemplateId": "eeitt_submission_confirmation",
           |  "authConfig": {
           |    "authModule": "anonymous"
           |  },
           |  "sections": [
           |    {
           |      "title": "Check before you start",
           |      "tasks": [
           |        {
           |          "id": "checkEligiblity",
           |          "title": "Check eligibility",
           |          "sections": [
           |            {
           |              "title": "Applicatns age details",
           |              "fields": [
           |                {
           |                  "id": "age",
           |                  "type": "text",
           |                  "format": "number",
           |                  "label": "Age"
           |                }
           |              ]
           |            }
           |          ]
           |        }
           |      ]
           |    }
           |  ],
           |  "acknowledgementSection": {
           |    "title": "Confirmation page ",
           |    "fields": []
           |  },
           |  "summarySection": {
           |    "title": "Check your answers"
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

    val expectedAnnotatedTaskList =
      Json
        .parse(
          """|{
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
             |    "acknowledgementSection": {
             |      "title": "Confirmation page ",
             |      "fields": [],
             |      "displayFeedbackLink": true
             |    }
             |  },
             |  "description": "",
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
             |    "type": "taskList",
             |    "sections": [
             |      {
             |        "title": "Check before you start",
             |        "tasks": [
             |          {
             |            "id": "checkEligiblity",
             |            "title": "Check eligibility",
             |            "sections": [
             |              {
             |                "title": "Applicatns age details",
             |                "fields": [
             |                  {
             |                    "id": "age",
             |                    "type": "text",
             |                    "format": "number",
             |                    "label": "Age"
             |                  }
             |                ]
             |              }
             |            ]
             |          }
             |        ]
             |      }
             |    ]
             |  },
             |  "summarySection": {
             |    "title": "Check your answers"
             |  },
             |  "formName": "Task List",
             |  "emailTemplateId": "eeitt_submission_confirmation",
             |  "_id": "foo",
             |  "originalId": "foo",
             |  "displayHMRCLogo": false,
             |  "parentFormSubmissionRefs": []
             |}""".stripMargin
        )
        .as[JsObject]

    assertEquals(
      FormTemplatesControllerRequestHandler.normaliseJSON(formKindTaskListInput).map(Json.prettyPrint),
      JsSuccess(Json.prettyPrint(expectedAnnotatedTaskList))
    )

  }
}
