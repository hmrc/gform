/*
 * Copyright 2019 HM Revenue & Customs
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
import play.api.libs.json._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.formtemplate.FormTemplatesControllerRequestHandler

class FormTemplateJSONSpec extends Spec {

  "Form template controller request handler" should "create a default language field in an empty json" in {
    val result = FormTemplatesControllerRequestHandler.backwardsCompatibleLanguage(Json.obj())
    val expectedResult = Json.obj("languages" -> List("en"))
    result shouldBe expectedResult
  }

  it should "not add a language field if provided one already" in {
    val expectedResults = Json.obj("languages" -> List("en", "cy"))
    val result = FormTemplatesControllerRequestHandler.backwardsCompatibleLanguage(expectedResults)
    result shouldBe expectedResults
  }

  it should "add a language field if not provided one" in {
    val result = FormTemplatesControllerRequestHandler.backwardsCompatibleLanguage(Json.obj("abc" -> "hello"))
    val expectedResult = Json.obj("abc" -> "hello", "languages" -> List("en"))
    result shouldBe expectedResult
  }

  "Form template Json" should "parse successfully with languages" in {
    val json: JsValue = Json.parse(requestBodyWithLanguages("${user.enrolledIdentifier}"))
    val jsResult: JsResult[FormTemplate] = implicitly[Reads[FormTemplate]].reads(json)
    jsResult.isSuccess shouldBe true
  }

  private def requestBodyWithLanguages(identifier: String, serviceId: Option[String] = Some(""""serviceId": "Id",""")) =
    s"""{
       |  "_id": "newfield",
       |  "formName": "Testing section change label tttt",
       |  "description": "Testing the form change label",
       |  "languages" : ["en"],
       |  "dmsSubmission": {
       |    "dmsFormId": "",
       |    "customerId": "$${auth.payenino}",
       |    "classificationType": "",
       |    "businessArea": ""
       |  },
       |  "authConfig": {
       |    "authModule": "hmrc",
       |    ${serviceId.getOrElse("")}
       |    "agentAccess": "allowAnyAgentAffinityUser"
       |  },
       |  "emailTemplateId": "",
       |  "submitSuccessUrl": "",
       |  "submitErrorUrl": "",
       |  "sections": [{
       |    "title": "Page A",
       |    "fields": [{
       |      "id": "elementA",
       |      "type": "text",
       |      "format": "sterling",
       |      "value": "$identifier",
       |      "submitMode": "readonly",
       |      "label": "Element A"
       |    }]
       |  }],
       |
       |  "declarationSection": {
       |    "title": "",
       |    "fields": []
       |  },
       |  "acknowledgementSection": {
       |    "title": "",
       |    "fields": []
       |  }
       |}""".stripMargin
}
