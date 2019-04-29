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

package uk.gov.hmrc.gform.sharedmodel

import org.scalatest.time.{ Millis, Seconds, Span }
import play.api.libs.json.{ JsError, JsResult, JsSuccess, Json }
import scala.io.Source
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formtemplate.Verifier
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate

class FormComponentRejectSpec extends Spec {

  implicit val defaultPatience = PatienceConfig(timeout = Span(6, Seconds), interval = Span(5, Millis))

  it should "reject all invalid templates" in {
    val table = Table(
      // format: off
      ("json",                                                           "expectedMessage"),
      ("revealing-choice-at-least-one-choice",                           "RevealingChoice error: At least one choice needs to be specified"),
      ("revealing-choice-mismatch-between-choices-and-revealing-fields", "RevealingChoice error: Number of 'choices': 2 and number of 'revealingFields': 3 does not match. They need to be identical."),
      ("revealing-choice-invalid-selection-index",                       "RevealingChoice error: Selection index 2 doesn't correspond to any of the choices"),
      ("revealing-choice-invalid-selections",                            "RevealingChoice error: Only single choice can be selected, but 0,1 has been defined")
      // format: on
    )

    forAll(table) {
      case (fileName, expectedMessage) =>
        val source = readFile(fileName)
        formtemplate.verifyReadFailure[FormTemplate](expectedMessage, source)
    }
  }

  it should "reject syntactically valid templates violating specification constraints" in {
    val table = Table(
      // format: off
      ("json",                                      "expectedMessage"),
      ("revealing-choice-include-revealing-choice", "Revealing choice cannot contains revealing choice as its element"),
      ("revealing-choice-include-group",            "Revealing choice cannot contains group as its element"),
      ("group-include-group",                       "Group cannot contains group as its element"),
      ("group-include-revealing-choice",            "Group cannot contains revealing choice as its element"),
      ("revealing-choice-unique-ids",               "Some FieldIds are defined more than once: List(tradingName)")
      // format: on
    )

    forAll(table) {
      case (fileName, expectedMessage) =>
        val jsResult = readAsFormTemplate(fileName)
        jsResult match {
          case JsSuccess(formTemplate, _) =>
            val verificationResult: FOpt[Unit] = new Verifier {}.verify(formTemplate)
            verificationResult.value.futureValue shouldBe Left(UnexpectedState(expectedMessage))
          case JsError(errors) => fail("Invalid formTemplate definition: " + errors)
        }
    }
  }

  private def readFile(fileName: String): String =
    Source.fromFile(s"test/resources/templates-to-reject/$fileName.json").mkString

  private def readAsFormTemplate(fileName: String): JsResult[FormTemplate] = {
    val source = readFile(fileName)
    val json = Json.parse(source)
    FormTemplate.format.reads(json)
  }

}
