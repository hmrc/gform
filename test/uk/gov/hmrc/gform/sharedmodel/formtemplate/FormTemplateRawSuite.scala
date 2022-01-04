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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import munit.FunSuite
import play.api.libs.json.{ JsObject, Json }

class FormTemplateRawSuite extends FunSuite {
  test("FormTemplateRaw.lowerCaseId should change _id to lowercase") {

    val json =
      Json
        .parse(
          """|{
             |  "_id": "Upper-CASE-Template-Id",
             |  "formName": "Lower case _id"
             |}""".stripMargin
        )
        .as[JsObject]

    val expected =
      Json
        .parse(
          """|{
             |  "_id": "upper-case-template-id",
             |  "formName": "Lower case _id"
             |}""".stripMargin
        )
        .as[JsObject]

    FormTemplateRaw(json).lowerCaseId.value shouldBe expected
  }
}
