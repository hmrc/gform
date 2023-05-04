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
      "_id": "gform-builder",
      "formName": "Gform Builder",
      "emailTemplateId": "confirmation",
      "authConfig": {
        "authModule": "anonymous"
      },
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
      ],
      "declarationSection": {
        "title": "Declaration",
        "fields": []
      },
      "acknowledgementSection": {
        "title": "Acknowledgement Page",
        "fields": []
      },
      "destinations": [
        {
          "id": "transitionToSubmitted",
          "type": "stateTransition",
          "requiredState": "Submitted"
        }
      ]
    }"""

  val expectedJson: Json = json"""
    {
      "_id": "gform-builder",
      "formName": "Gform Builder",
      "emailTemplateId": "confirmation",
      "authConfig": {
        "authModule": "anonymous"
      },
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
      ],
      "declarationSection": {
        "title": "Declaration",
        "fields": []
      },
      "acknowledgementSection": {
        "title": "Acknowledgement Page",
        "fields": []
      },
      "destinations": [
        {
          "id": "transitionToSubmitted",
          "type": "stateTransition",
          "requiredState": "Submitted"
        }
      ]
    }"""

  val expectedJson2: Json = json"""
    {
      "_id": "gform-builder",
      "formName": "Gform Builder",
      "emailTemplateId": "confirmation",
      "authConfig": {
        "authModule": "anonymous"
      },
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
      ],
      "declarationSection": {
        "title": "Declaration",
        "fields": []
      },
      "acknowledgementSection": {
        "title": "Acknowledgement Page",
        "fields": []
      },
      "destinations": [
        {
          "id": "transitionToSubmitted",
          "type": "stateTransition",
          "requiredState": "Submitted"
        }
      ]
    }"""

  test("Update section title (1)") {
    val patch: Json = Json.obj("title" := "Page AA")

    val result = BuilderSupport.modifySectionData(json, 0, patch)

    assertEquals(result, expectedJson)
  }

  test("Update section title (2)") {
    val patch: Json = Json.obj("title" := "Page AA")

    val result = BuilderSupport.modifySectionData(json, 1, patch)

    assertEquals(result, expectedJson2)
  }

  val formComponentUpdateJson: Json = json"""
    {
      "_id": "gform-builder",
      "formName": "Gform Builder",
      "emailTemplateId": "confirmation",
      "authConfig": {
        "authModule": "anonymous"
      },
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
      ],
      "declarationSection": {
        "title": "Declaration",
        "fields": []
      },
      "acknowledgementSection": {
        "title": "Acknowledgement Page",
        "fields": []
      },
      "destinations": [
        {
          "id": "transitionToSubmitted",
          "type": "stateTransition",
          "requiredState": "Submitted"
        }
      ]
    }"""

  val formComponentUpdateJsonExpected: Json = json"""
    {
      "_id": "gform-builder",
      "formName": "Gform Builder",
      "emailTemplateId": "confirmation",
      "authConfig": {
        "authModule": "anonymous"
      },
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
      ],
      "declarationSection": {
        "title": "Declaration",
        "fields": []
      },
      "acknowledgementSection": {
        "title": "Acknowledgement Page",
        "fields": []
      },
      "destinations": [
        {
          "id": "transitionToSubmitted",
          "type": "stateTransition",
          "requiredState": "Submitted"
        }
      ]
    }"""

  val formComponentUpdateHelpTextJsonExpected: Json = json"""
    {
      "_id": "gform-builder",
      "formName": "Gform Builder",
      "emailTemplateId": "confirmation",
      "authConfig": {
        "authModule": "anonymous"
      },
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
      ],
      "declarationSection": {
        "title": "Declaration",
        "fields": []
      },
      "acknowledgementSection": {
        "title": "Acknowledgement Page",
        "fields": []
      },
      "destinations": [
        {
          "id": "transitionToSubmitted",
          "type": "stateTransition",
          "requiredState": "Submitted"
        }
      ]
    }"""

  test("Update field label (1)") {
    val formComponentId = FormComponentId("firstName")

    val patch: Json = Json.obj("label" := "First name 22")

    val result: Json = BuilderSupport.modifyFormComponentData(formComponentUpdateJson, 1, formComponentId, patch)

    assertEquals(result, formComponentUpdateJsonExpected)
  }

  test("Update field helpText (1)") {
    val formComponentId = FormComponentId("firstName")

    val patch: Json = Json.obj("helpText" := "I'm help text")

    val result: Json = BuilderSupport.modifyFormComponentData(formComponentUpdateJson, 1, formComponentId, patch)

    assertEquals(result, formComponentUpdateHelpTextJsonExpected)
  }
}