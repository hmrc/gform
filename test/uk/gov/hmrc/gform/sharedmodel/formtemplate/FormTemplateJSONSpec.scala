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

  "normaliseJSON" should "ensure default values for missing fields" in {

    val emptyInput = Json.obj()
    val emptyExpected = Json.obj(
      "formCategory" -> "default",
      "languages"    -> Json.arr("en"),
      "draftRetrievalMethod" -> Json.obj(
        "value"                    -> "onePerUser",
        "showContinueOrDeletePage" -> "true"
      ))

    val input = Json
      .obj(
        "abc"                      -> "hello",
        "formCategory"             -> "letter",
        "languages"                -> Json.arr("en", "cy"),
        "draftRetrievalMethod"     -> "formAccessCodeForAgents",
        "showContinueOrDeletePage" -> "false"
      )
    val expected = Json.obj(
      "abc"          -> "hello",
      "formCategory" -> "letter",
      "languages"    -> Json.arr("en", "cy"),
      "draftRetrievalMethod" -> Json.obj(
        "value"                    -> "formAccessCodeForAgents",
        "showContinueOrDeletePage" -> "false"
      )
    )

    val t = Table(
      ("input", "expected"),
      (emptyInput, emptyExpected),
      (input, expected)
    )

    forAll(t) {
      case (input, expected) =>
        val result = FormTemplatesControllerRequestHandler.normaliseJSON(input)
        result should beJsSuccess(expected)
    }
  }
}
