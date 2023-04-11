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
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.LocalisedString
import uk.gov.hmrc.gform.sharedmodel.LangADT

class DeserializeEmailCodeParameterSpec extends FunSuite {

  test("Deserialize JSON to EmailCodeParameter") {
    val json =
      """|{
         |  "value": {
         |    "en": "English value",
         |    "cy": "Welsh value"
         |  },
         |  "emailTemplateVariable": "testTemplateVar"
         |}""".stripMargin
    val result = implicitly[Reads[EmailCodeParameter]].reads(Json.parse(json))
    result shouldBe JsSuccess(
      EmailCodeParameter(
        "testTemplateVar",
        LocalisedString(Map(LangADT.En -> "English value", LangADT.Cy -> "Welsh value"))
      )
    )
  }

}
