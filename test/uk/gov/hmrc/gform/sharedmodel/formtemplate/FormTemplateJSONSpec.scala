/*
 * Copyright 2020 HM Revenue & Customs
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

    val printSectionInput = Json.obj(
      "printSection" -> "TestPrintSection"
    )
    val printSectionExpected = Json.obj(
      "formCategory" -> "default",
      "languages"    -> Json.arr("en"),
      "destinations" -> "TestPrintSection",
      "draftRetrievalMethod" -> Json.obj(
        "value"                    -> "onePerUser",
        "showContinueOrDeletePage" -> "true"
      ),
      "parentFormSubmissionRefs" -> Json.arr()
    )

    val destinationsInput = Json.obj(
      "destinations" -> Json.obj(
        "id"            -> "transitionToSubmitted",
        "type"          -> "stateTransition",
        "requiredState" -> "Submitted"
      )
    )

    val destinationsExpected = Json.obj(
      "formCategory" -> "default",
      "languages"    -> Json.arr("en"),
      "destinations" -> Json.obj(
        "id"            -> "transitionToSubmitted",
        "type"          -> "stateTransition",
        "requiredState" -> "Submitted"
      ),
      "draftRetrievalMethod" -> Json.obj(
        "value"                    -> "onePerUser",
        "showContinueOrDeletePage" -> "true"
      ),
      "parentFormSubmissionRefs" -> Json.arr()
    )

    val input = Json
      .obj(
        "testText" -> "hello",
        "testJsonObj" -> Json.obj(
          "id"            -> "transitionToSubmitted",
          "type"          -> "stateTransition",
          "requiredState" -> "Submitted"
        ),
        "testJsonArr"              -> Json.arr("en", "cy"),
        "formCategory"             -> "letter",
        "languages"                -> Json.arr("en", "cy"),
        "printSection"             -> "TestPrintSection",
        "draftRetrievalMethod"     -> "formAccessCodeForAgents",
        "showContinueOrDeletePage" -> "false",
        "parentFormSubmissionRefs" -> Json.arr("123", "456")
      )

    val expected = Json.obj(
      "testText" -> "hello",
      "testJsonObj" -> Json.obj(
        "id"            -> "transitionToSubmitted",
        "type"          -> "stateTransition",
        "requiredState" -> "Submitted"
      ),
      "testJsonArr"  -> Json.arr("en", "cy"),
      "formCategory" -> "letter",
      "languages"    -> Json.arr("en", "cy"),
      "destinations" -> "TestPrintSection",
      "draftRetrievalMethod" -> Json.obj(
        "value"                    -> "formAccessCodeForAgents",
        "showContinueOrDeletePage" -> "false"
      ),
      "parentFormSubmissionRefs" -> Json.arr("123", "456")
    )

    val t = Table(
      ("input", "expected"),
      (printSectionInput, printSectionExpected),
      (destinationsInput, destinationsExpected),
      (input, expected)
    )

    forAll(t) {
      case (input, expected) =>
        val result = FormTemplatesControllerRequestHandler.normaliseJSON(input)
        result should beJsSuccess(expected)
    }
  }

  it should "return validation error when both destinations and printSection are present" in {

    val input = Json.obj(
      "printSection" -> "TestPrintSection",
      "destinations" -> Json.obj(
        "id"            -> "transitionToSubmitted",
        "type"          -> "stateTransition",
        "requiredState" -> "Submitted"
      )
    )

    FormTemplatesControllerRequestHandler.normaliseJSON(input) should be(
      FormTemplatesControllerRequestHandler.onlyOneOfDestinationsAndPrintSection)
  }

  it should "return validation error when both destinations and printSection are missing" in {

    val input = Json.obj()

    FormTemplatesControllerRequestHandler.normaliseJSON(input) should be(
      FormTemplatesControllerRequestHandler.onlyOneOfDestinationsAndPrintSection)
  }

}
