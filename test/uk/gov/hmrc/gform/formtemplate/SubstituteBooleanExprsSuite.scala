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

package uk.gov.hmrc.gform.formtemplate

import munit.FunSuite
import play.api.libs.json.{ JsError, JsObject, JsSuccess, Json }
import scala.language.implicitConversions
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class SubstituteBooleanExprsSuite extends FunSuite with FormTemplateSupport {

  implicit def stringToFormComponentId(str: String): FormComponentId = FormComponentId(str)

  val substituteBooleanEpxrs = new SubstituteBooleanExprs {}

  test("SubstituteBooleanExprs should substitute includeIf") {
    val jsonStr =
      """|{
         |  "_id": "top-level-expressions",
         |  "formName": "Top level",
         |  "version": 1,
         |  "description": "",
         |  "emailTemplateId": "",
         |  "authConfig": {
         |    "authModule": "anonymous"
         |  },
         |  "expressions": {
         |    "zero": "0"
         |  },
         |  "booleanExpressions": {
         |    "myBoolExpr": "AAA > zero"
         |  },
         |  "sections": [
         |    {
         |      "title": "Section A",
         |      "includeIf": "${myBoolExpr}",
         |      "fields": []
         |    },
         |    {
         |      "title": "Section B",
         |      "includeIf": "${myBoolExpr && AAA < 100}",
         |      "fields": []
         |    }
         |  ],
         |  "declarationSection": {
         |    "title": "Declaration",
         |    "fields": []
         |  },
         |  "acknowledgementSection": {
         |    "title": "Confirmation page ",
         |    "fields": []
         |  },
         |  "destinations": [
         |    {
         |      "id": "transitionToSubmitted",
         |      "type": "stateTransition",
         |      "requiredState": "Submitted"
         |    }
         |  ]
         |}""".stripMargin

    toFormTemplateAndSubstitutions(jsonStr) { (formTemplate, substitutions, expSubstitutions) =>
      val res = substituteBooleanEpxrs.substituteBooleanExprs(formTemplate, substitutions, expSubstitutions)
      val sectionA = res.formKind.allSections.head
      sectionA match {
        case Section.NonRepeatingPage(page) =>
          val expectedBooleanExpr = IncludeIf(GreaterThan(FormCtx("AAA"), Constant("0")))
          assertEquals(page.includeIf, Some(expectedBooleanExpr))
        case unexpectedSection => fail(s"Unexpected Section $unexpectedSection")
      }
      val sectionB = res.formKind.allSections(1)
      sectionB match {
        case Section.NonRepeatingPage(page) =>
          val expectedBooleanExpr =
            IncludeIf(And(GreaterThan(FormCtx("AAA"), Constant("0")), LessThan(FormCtx("AAA"), Constant("100"))))
          assertEquals(page.includeIf, Some(expectedBooleanExpr))
        case unexpectedSection => fail(s"Unexpected Section $unexpectedSection")
      }
    }
  }

  private def toFormTemplateAndSubstitutions[A](
    jsonStr: String
  )(f: (FormTemplate, BooleanExprSubstitutions, ExprSubstitutions) => A) = {
    val maybeNormalisedJson = FormTemplatesControllerRequestHandler.normaliseJSON(Json.parse(jsonStr))

    maybeNormalisedJson match {
      case JsError(error) => fail(s"Failed to normalise json $error")
      case JsSuccess(normalisedJson, _) =>
        val formTemplate: FormTemplate = normalisedJson.as[FormTemplate]
        val maybeBooleanExprSubstitutions: Opt[BooleanExprSubstitutions] =
          BooleanExprSubstitutions.from(FormTemplateRaw(normalisedJson.as[JsObject]))
        val maybeExprSubstitutions: Opt[ExprSubstitutions] =
          ExprSubstitutions.from(FormTemplateRaw(normalisedJson.as[JsObject]))

        (for {
          booleanExprSubstitutions <- maybeBooleanExprSubstitutions
          exprSubstitutions        <- maybeExprSubstitutions
        } yield f(formTemplate, booleanExprSubstitutions, exprSubstitutions)) match {
          case Left(unexpectedState) => fail(unexpectedState.error)
          case Right(result)         => result
        }
    }
  }
}
