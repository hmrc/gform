package uk.gov.hmrc.gform.it.sample

import play.api.libs.json.{ JsString, JsValue, Json }

trait FormTemplateSample {

  def basicFormTemplate(id: String = "basic", emailTemplateId: JsValue = JsString("email_template_id")) =
    Json.parse(s"""
                  |{
                  |    "_id": "$id",
                  |    "formName": "Test form name",
                  |    "version": "1",
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
