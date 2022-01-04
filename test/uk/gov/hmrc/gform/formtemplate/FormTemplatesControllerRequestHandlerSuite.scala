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

import munit.{ FunSuite, Location }
import play.api.libs.json.{ JsDefined, JsString, JsSuccess, JsValue, Json }

class FormTemplatesControllerRequestHandlerSuite extends FunSuite {
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

    val JsSuccess(normalised, _) = FormTemplatesControllerRequestHandler.normaliseJSON(json)

    assertFieldValue("_id", "upper-case-template-id", normalised)
    assertFieldValue("originalId", "Upper-CASE-Template-Id", normalised)

  }

  private def assertFieldValue(fieldName: String, expectedValue: String, json: JsValue)(implicit loc: Location) =
    json \ fieldName match {
      case JsDefined(JsString(str)) => assertEquals(str, expectedValue)
      case otherwise                => fail(s"$fieldName is undefined or not a string: $otherwise")
    }

}
