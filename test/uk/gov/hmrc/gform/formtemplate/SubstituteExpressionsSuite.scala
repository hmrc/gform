/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._, OffsetUnit.Year, OffsetUnit.Day, OffsetUnit.Month

class SubstituteExpressionsSuite extends FunSuite with FormTemplateSupport {

  implicit def stringToFormComponentId(str: String): FormComponentId = FormComponentId(str)

  val substituteExpressions = new SubstituteExpressions {}

  def smartStringWithExpr(expr: Expr) = SmartString(
    LocalisedString(Map(LangADT.En -> "stringEn", LangADT.Cy -> "stringCy")),
    List(expr)
  )

  def sectionWithSmartString(smartString: SmartString) = mkSectionNonRepeatingPage(
    name = "section1",
    formComponents = List(
      mkFormComponent(
        "section1Component1",
        Some(Instruction(Some(smartString), Some(1)))
      )
    ),
    Some(Instruction(Some(smartString), Some(1)))
  )

  test("SubstituteExpressions should substitute FormCtx references with corresponding substitute expressions") {

    val sustitute = Add(Constant("1"), Constant("2"))

    val substitutions = Substitutions(
      Map(ExpressionId("isHappy") -> sustitute)
    )

    val title = smartStringWithExpr(FormCtx(FormComponentId("isHappy")))
    val titleSubstituted = smartStringWithExpr(sustitute)

    val section = sectionWithSmartString(title)
    val sectionSubstituted = sectionWithSmartString(titleSubstituted)

    val formTemplate = mkFormTemplate(List(section))
    val formTemplateExpected = mkFormTemplate(List(sectionSubstituted))

    val substituted = substituteExpressions.substituteExpressions(formTemplate, substitutions)

    assertEquals(substituted.sections, formTemplateExpected.sections)
  }

  test("SubstituteExpressions should allow alias DateExpr fragments") {
    val jsonStr =
      """|{
         |  "_id": "top-level-expressions",
         |  "formName": "Top level",
         |  "description": "",
         |  "emailTemplateId": "",
         |  "authConfig": {
         |    "authModule": "anonymous"
         |  },
         |  "expressions": {
         |    "claimDateOnly": "claimDate",
         |    "claimDateYAgo": "claimDate -1y",
         |    "claimDateYMAgo": "claimDate -1y-1m",
         |    "claimDateYMDAgo": "claimDate -1y-1m-1d",
         |    "todayOnly": "TODAY",
         |    "todayY": "TODAY+2y",
         |    "todayYM": "TODAY+2y+3m",
         |    "todayYMD": "TODAY+2y+3m+4d",
         |    "concrete": "'31 12 2014'"
         |  },
         |  "sections": [
         |    {
         |      "title": "${claimDateOnly -1y-1m-1d} ${claimDate -1y-1m-1d} ${claimDateYAgo-1m-1d} ${claimDateYMAgo-1d} ${claimDateYMDAgo} ${TODAY+2y+3m+4d} ${todayOnly+2y+3m+4d} ${todayY+3m+4d} ${todayYM+4d} ${todayYMD} ${concrete}",
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

    toFormTemplateAndSubstitutions(jsonStr) { (formTemplate, substitutions) =>
      val res = substituteExpressions.substituteExpressions(formTemplate, substitutions)
      val section = res.sections(0)
      section match {
        case Section.NonRepeatingPage(page) =>
          val expectedDateCtx = DateCtx(
            DateExprWithOffset(
              DateFormCtxVar(FormCtx("claimDate")),
              OffsetYMD(List(Year(-1), Month(-1), Day(-1)))
            )
          )

          val expectedTodayCtx = DateCtx(
            DateExprWithOffset(DateValueExpr(TodayDateExprValue), OffsetYMD(List(Year(2), Month(3), Day(4))))
          )
          val expected = List(
            expectedDateCtx,
            expectedDateCtx,
            expectedDateCtx,
            expectedDateCtx,
            expectedDateCtx,
            expectedTodayCtx,
            expectedTodayCtx,
            expectedTodayCtx,
            expectedTodayCtx,
            expectedTodayCtx,
            DateCtx(DateValueExpr(ExactDateExprValue(2014, 12, 31)))
          )
          assertEquals(page.title.interpolations, expected)
        case unexpectedSection => fail(s"Unexpected Section $unexpectedSection")
      }
    }
  }

  test("SubstituteExpressions should allow to make an alias") {
    val jsonStr =
      """|{
         |  "_id": "top-level-expressions",
         |  "formName": "Top level",
         |  "description": "",
         |  "emailTemplateId": "",
         |  "authConfig": {
         |    "authModule": "anonymous"
         |  },
         |  "expressions": {
         |    "fooPlusBar": "foo + bar"
         |  },
         |  "sections": [
         |    {
         |      "title": "${a + (foo + bar) + c} ${a + fooPlusBar + c}",
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

    toFormTemplateAndSubstitutions(jsonStr) { (formTemplate, substitutions) =>
      val res = substituteExpressions.substituteExpressions(formTemplate, substitutions)
      val section = res.sections(0)
      section match {
        case Section.NonRepeatingPage(page) =>
          val expectedAdd = Add(Add(FormCtx("a"), Add(FormCtx("foo"), FormCtx("bar"))), FormCtx("c"))

          val expected = List(
            expectedAdd,
            expectedAdd
          )
          assertEquals(page.title.interpolations, expected)
        case unexpectedSection => fail(s"Unexpected Section $unexpectedSection")
      }
    }
  }

  private def toFormTemplateAndSubstitutions[A](jsonStr: String)(f: (FormTemplate, Substitutions) => A) = {
    val maybeNormalisedJson = FormTemplatesControllerRequestHandler.normaliseJSON(Json.parse(jsonStr))

    maybeNormalisedJson match {
      case JsError(error) => fail(s"Failed to normalise json $error")
      case JsSuccess(normalisedJson, _) =>
        val formTemplate: FormTemplate = normalisedJson.as[FormTemplate]
        val maybeSubstitutions: Opt[Substitutions] = Substitutions.from(FormTemplateRaw(normalisedJson.as[JsObject]))

        maybeSubstitutions match {
          case Left(unexpectedState) => fail(unexpectedState.error)
          case Right(substitutions)  => f(formTemplate, substitutions)
        }
    }
  }
}
