/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.it.sample

import play.api.libs.json.{ JsString, JsValue, Json }

trait FormTemplateSample {

  def basicFormTemplate(id: String = "basic", emailTemplateId: JsValue = JsString("email_template_id")) =
    Json.parse(s"""
                  |{
                  |    "_id": "$id",
                  |    "formName": "Test form name",
                  |    "version": 1,
                  |    "description": "Test form description",
                  |    "authConfig": {
                  |        "authModule": "anonymous"
                  |    },
                  |    "destinations": [
                  |        {
                  |            "id": "HMRCDMS",
                  |            "type": "hmrcDms",
                  |            "dmsFormId": "TSTHMRCDMS",
                  |            "customerId": "$${auth.gg}",
                  |            "classificationType": "ClassificationType",
                  |            "businessArea": "BusinessArea",
                  |            "roboticsXml": false
                  |        }
                  |    ],
                  |    "emailTemplateId": $emailTemplateId,
                  |    "userResearchUrl": "https://test.service.gov.uk",
                  |    "serviceStartPageUrl": "https://startpage.service.gov.uk",
                  |    "accessibilityUrl": "$id",
                  |    "sections": [
                  |        {
                  |            "title": "Page1",
                  |            "fields": [
                  |                {
                  |                    "id": "textField1",
                  |                    "type": "text",
                  |                    "label": "Text field 1",
                  |                    "format": "text"
                  |                }
                  |            ]
                  |        }
                  |    ],
                  |    "declarationSection": {
                  |        "title": "Declaration Page",
                  |        "fields": []
                  |    },
                  |    "acknowledgementSection": {
                  |        "title": "Acknowledgement Page",
                  |        "fields": []
                  |    }
                  |}
                  |""".stripMargin)

}
