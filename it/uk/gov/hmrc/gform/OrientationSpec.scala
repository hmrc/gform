package uk.gov.hmrc.gform

import play.api.http.Status
import play.api.libs.json._
import play.api.libs.ws.WSResponse
import uk.gov.hmrc.gform.models._


class OrientationSpec extends support.ITSpec {

  "The application should accept a template" in {

    val weReq = wsclient.url(s"$baseUrl/gform/formtemplates").withHeaders("Content-Type" -> "application/json")
    val jsonString =
      """{
          "schemaId": "http://hmrc.gov.uk/jsonschema/gf-formtemplate#",
          "formTypeId": "exprtest3",
          "formName": "Insurance Premium Tax Return | Yswiriant Ffurflen Dreth Premiwm",
          "version": "0.3.0",
          "description": "Fill in your insurance premium tax return form online | Llenwch eich ffurflen dreth premiwm yswiriant ar-lein",
          "characterSet": "UTF-8",
          "dmsSubmission": {
            "customerId": "nino",
            "classificationType": "BT-NRU-Environmental",
            "businessArea": "FinanceOpsCorpT"
          },
          "authConfig": {
            "authModule": "legacyEEITTAuth",
            "regimeId": "GF"
          },
          "submitSuccessUrl": "http://www.google.co.uk",
          "submitErrorUrl": "http://www.yahoo.co.uk",
          "sections": [
            {
              "title": "Calculation 1/2 | eich calculationewq",
              "shortName": "shortName",
              "fields": [
                {
                  "type": "group",
                  "id": "gid",
                  "label": "glabel",
                  "format": "horizontal",
                  "repeatsMin": 1,
                  "repeatsMax": 5,
                  "repeatLabel": "repeatLabel",
                  "repeatAddAnotherText": "repeatAddAnotherText",
                  "fields": [
                    {
                      "type": "choice",
                      "id": "cid",
                      "label": "clabel",
                      "choices": [
                        "A",
                        "B"
                      ]
                    }
                  ],
                  "presentationHint": "collapseGroupUnderLabel,summariseGroupAsGrid"
                },
                {
                  "id": "amountA",
                  "label": "Amount A | Rhif A",
                  "mandatory": "true"
                }
              ]
            }
          ]
        }"""


    val wSResponse = eventually {
      weReq.post(jsonString).futureValue
    }
    wSResponse.status shouldBe Status.OK
    wSResponse.body shouldBe """{"success":"SUCCESSFUL"}"""

    val response: WSResponse = eventually {
      wsclient.url(s"$baseUrl/gform/formtemplates/exprtest3").get().futureValue
    }
    response.status shouldBe Status.OK

    val respString = response.body
    println("++++++++++++++++++++" + respString)

//    val jsValue = (Json.parse(respString) \\ "orientation").head
    val jsValue = Json.parse(respString)

    jsValue match {
      case JsObject(value) => {
        val jsResult: JsResult[FormTemplate] = implicitly[Reads[FormTemplate]].reads(JsObject(value))
        println("++++++++++")
        println(jsResult)

        val jsValue = (Json.parse(respString) \\ "orientation").head
        val res = jsValue.validate[Orientation]
        println(res)

        val jsValue2 = (Json.parse(respString) \\ "presentationHint").head
//        val res2 = jsValue2.validate[PresentationHint]
        println(jsValue2)
      }
      case _ => println("++++ unexpected")
    }

  }


  private lazy val baseUrl = s"http://localhost:${port}"
}
