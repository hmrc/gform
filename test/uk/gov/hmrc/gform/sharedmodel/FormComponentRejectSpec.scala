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

package uk.gov.hmrc.gform.sharedmodel

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.time.{ Millis, Seconds, Span }
import play.api.libs.json.{ JsError, JsResult, JsSuccess, Json }

import scala.io.Source
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formtemplate.{ ExprSubstitutions, Rewriter, Verifier }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate

class FormComponentRejectSpec extends Spec with TableDrivenPropertyChecks {

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

    forAll(table) { case (fileName, expectedMessage) =>
      val source = readFile(fileName)
      formtemplate.verifyReadFailure[FormTemplate](expectedMessage, source)
    }
  }

  it should "reject syntactically valid templates violating specification constraints" in {
    val table = Table(
      // format: off
      ("json",                                      "expectedMessage"),
      ("invalid-version",                           "version cannot contains uppercase letters. Found: V1"),
      ("invalid-invalid-legacy-form-ids",           "legacyFormIds must be lowercase. Found: Version-Test-1"),
      ("revealing-choice-include-revealing-choice", "Revealing choice cannot contains revealing choice as its element"),
      ("revealing-choice-include-group",            "Revealing choice cannot contains group as its element"),
      ("group-include-group",                       "Group cannot contains group as its element"),
      ("group-include-revealing-choice",            "Group cannot contains revealing choice as its element"),
      ("revealing-choice-unique-ids",               "Some FieldIds are defined more than once: List(companyName, tradingName)"),
      ("email-verification-forward-reference",      "id 'code' named in email verification is forward reference, which is not permitted"),
      ("email-verification-invalid-reference",      "id 'code2' named in email verification does not exist in the form"),
      ("confirming-invalid-page-id",                "No pageId 'bankAccountPage' found. Confirmation question 'confirmationQuestion' should confirm pageId: 'bankAccountPage'"),
      ("postcode-with-other-component",             "Postcode lookup 'postcode' cannot be on the page with other components (exception are info components)"),
      ("postcode-multiple-instances",               "Only one Postcode lookup on a page is allowed. Some page has 2 postcode lookups: postcode, postcode2"),
      ("postcode-in-revealing-choice",              "Postcode lookup 'postcode' cannot be inside Revealing choice or Group"),
      ("choice-options-uniformness",                "Choice component has some options with 'value' and some options without 'value': hasDifferentName"),
      ("choice-options-uniformness-in-rc",          "Choice component has some options with 'value' and some options without 'value': hasDifferentName"),
      ("choice-options-unique",                     "Choice component options 'value's needs to be unique: hasDifferentName"),
      ("choice-options-unique-in-rc",               "Choice component options 'value's needs to be unique: hasDifferentName"),
      ("revealing-choice-options-uniformness",      "Revealing choice component has some choices with 'value' and some choices without 'value': companyName"),
      ("revealing-choice-options-unique",           "Revealing choice component choices 'value's needs to be unique: companyName"),
      ("choice-index-size-expr-with-value",         "Expression 'hasDifferentName.0.size' can't use numeric index: 0. hasDifferentName has these values: foo, bar"),
      ("choice-value-size-expr-with-index",         "Expression 'hasDifferentName.A.size' can't use value index: A. Use numeric index from 0 to 1"),
      ("choice-value-size-expr-wrong-value",        "Expression 'hasDifferentName.baz.size' has wrong value baz. hasDifferentName has these values: foo, bar")
      // format: on",
    )

    forAll(table) { case (fileName, expectedMessage) =>
      val jsResult = readAsFormTemplate(fileName)
      jsResult match {
        case JsSuccess(formTemplate, _) =>
          val verificationResult: FOpt[Unit] = new Verifier {}.verify(formTemplate)(ExprSubstitutions.empty)
          verificationResult.value.futureValue shouldBe Left(UnexpectedState(expectedMessage))
        case JsError(errors) => fail("Invalid formTemplate definition: " + errors)
      }
    }
  }

  it should "reject syntactically valid templates violating specification rewrite" in {
    val table = Table(
      // format: off
      ("json",                             "expectedMessage"),
      ("choice-wrong-index",               "Expression 'fcChoice contains 10' has wrong index 10. Choice fcChoice has only 2 elements. Use index from 0 to 1"),
      ("choice-wrong-index-equals",        "Expression 'simpleChoice = 10' has wrong index 10. Choice simpleChoice has only 2 elements. Use index from 0 to 1"),
      ("choice-multivalue-equals",         "Multivalue choice cannot be used together with '='. Replace 'simpleChoice = 1' with 'simpleChoice contains 1' instead."),
      ("choice-multivalue-equals-swapped", "Multivalue choice cannot be used together with '='. Replace '1 = simpleChoice' with 'simpleChoice contains 1' instead."),
      ("revealing-choice-wrong-index",     "Expression 'rChoice contains 10' has wrong index 10. Revealing choice rChoice has only 2 elements. Use index from 0 to 1"),
      ("component-level-include-if",       "Multivalue choice cannot be used together with '='. Replace 'choice = 0' with 'choice contains 0' instead."),
      ("choice-options-value-in-expr-1",     "Expression 'hasDifferentName contains baz' is invalid. 'baz' is not one of the possible values: foo, bar"),
      ("choice-options-value-in-expr-2",     "Expression 'hasDifferentName contains baz' is invalid. 'baz' is not one of the possible values: foo, bar"),
      ("choice-options-value-in-expr-3",     "Expression 'trading contains baz' is invalid. 'baz' is not one of the possible values: foo, bar"),
      // format: on
    )

    forAll(table) { case (fileName, expectedMessage) =>
      val jsResult = readAsFormTemplate(fileName)
      jsResult match {
        case JsSuccess(formTemplate, _) =>
          val verificationResult: FOpt[FormTemplate] = new Rewriter {}.rewrite(formTemplate)
          verificationResult.value.futureValue shouldBe Left(UnexpectedState(expectedMessage))
        case JsError(errors) => fail("Invalid formTemplate definition: " + errors)
      }
    }
  }

  it should "reject template containing invalid reference field" in {
    val table = Table(
      // format: off
      ("json",                                     "expectedMessage"),
      ("invalid-count-usage",                      "sections.fields.[id=lastInfo].infoText: textA cannot be use with .count function. Only AddToList id can be used with .count"),
      ("invalid-sum-usage",                        "sections.fields.[id=lastInfo].infoText: textA cannot be use with .sum function. Only numeric fields from Group component, Repeated section or AddToList section can be used with .sum function"),
      ("invalid-addtolist-cross-reference",        "title: fieldA belongs to different Add To List section"),
      ("invalid-repeated-section-cross-reference", "fields.[id=fieldB].label: fieldA belongs to different Repeated section"),
      ("invalid-group-cross-reference",            "fields.[id=groupB].[id=fieldB].label: fieldA belongs to different Group component"),
      ("nonexistent-field-reference-in-if-else",   "sections.fields.[id=textA].label: foo doesn't exist in the form"),
      ("nonexistent-field-reference",              "sections.fields.[id=textA].label: textB doesn't exist in the form"),
      ("empty-shortname-non-repeated",             "shortName is empty for title: 'Page A'. If you want to hide page title on summary page, use 'presentationHint': 'invisiblePageTitle' instead."),
      ("empty-shortname-repeated",                 "shortName is empty for title: 'Page A'. If you want to hide page title on summary page, use 'presentationHint': 'invisiblePageTitle' instead."),
      ("empty-shortname-add-to-list",              "shortName is empty for title: 'Page $n'. If you want to hide page title on summary page, use 'presentationHint': 'invisiblePageTitle' instead."),
      // format: on
    )

    forAll(table) { case (fileName, expectedMessage) =>
      val jsResult = readAsFormTemplate(fileName)
      jsResult match {
        case JsSuccess(formTemplate, _) =>
          val verificationResult: FOpt[Unit] = new Verifier {}.verify(formTemplate)(ExprSubstitutions.empty)
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
    FormTemplate.transformAndReads(json)
  }

}
