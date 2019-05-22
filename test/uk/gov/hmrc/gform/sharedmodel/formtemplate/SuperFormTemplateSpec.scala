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

import cats.data.NonEmptyList
import play.api.libs.json.Json
import uk.gov.hmrc.gform.Spec

class SuperFormTemplateSpec extends Spec {

  it should "serialise/deserialize to json a SuperFormTemplate" in {
    val section = SuperFormSection(FormTemplateId("456"), "title", "mainDispalyField", "secondary", Option(false))
    val superForm = SuperFormTemplate(
      FormTemplateId("123"),
      "title",
      "description",
      "superFormId",
      "label",
      NonEmptyList.one(section)
    )

    Json.parse(superFormJson).as[SuperFormTemplate] shouldBe superForm
    Json.toJson(superForm).validate[SuperFormTemplate].asOpt.get should matchPattern {
      case SuperFormTemplate(_, _, _, _, _, _) =>
    }
  }

  private val superFormJson =
    s"""{
       |  "_id": "123",
       |  "title": "title",
       |  "description": "description",
       |  "superFormIdPrefix": "superFormId",
       |  "superFormIdLabel": "label",
       |  "sections": [
       |    {
       |      "_id": "456",
       |      "title": "title",
       |      "mainDisplayField": "mainDispalyField",
       |      "secondaryDisplayField": "secondary",
       |      "repeats": false
       |    }
       |  ]
       |}""".stripMargin
}
