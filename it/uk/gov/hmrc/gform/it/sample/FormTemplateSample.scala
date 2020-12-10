package uk.gov.hmrc.gform.it.sample

import play.api.libs.json.Json

trait FormTemplateSample {
  val basicFormTemplate = Json.parse("""
                                       |{
                                       |    "_id": "BASIC",
                                       |    "formName": "Test form name",
                                       |    "description": "Test form description",
                                       |    "authConfig": {
                                       |        "authModule": "anonymous"
                                       |    },
                                       |    "destinations": [
                                       |        {
                                       |            "id": "HMRCDMS",
                                       |            "type": "hmrcDms",
                                       |            "dmsFormId": "TSTHMRCDMS",
                                       |            "customerId": "${auth.gg}",
                                       |            "classificationType": "ClassificationType",
                                       |            "businessArea": "BusinessArea",
                                       |            "roboticsXml": false
                                       |        }
                                       |    ],
                                       |    "emailTemplateId": "email_template_id",
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
