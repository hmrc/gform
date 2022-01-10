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

package uk.gov.hmrc.gform.formtemplate

import munit.FunSuite
import play.api.libs.json.{ JsObject, Json }
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class ExprSubstitutionsSuite extends FunSuite {

  private def toSubstitutions(jsonStr: String): Opt[ExprSubstitutions] = {
    val json: JsObject = Json.parse(jsonStr).as[JsObject]

    ExprSubstitutions.from(FormTemplateRaw(json))
  }

  test(
    "ExprSubstitutions.from should return empty ExprSubstitutions when top-level field 'expressions' is not present in the json"
  ) {
    val inputJson = "{}"

    val res: Opt[ExprSubstitutions] = toSubstitutions(inputJson)

    assertEquals(res, Right(ExprSubstitutions.empty))
  }

  test("ExprSubstitutions.from should parse valid expressions") {
    val inputJson =
      """|{
         |  "expressions": {
         |    "isHappy": "if textBox1 match '[a-zA-Z0-9]{6}' then 'happy' else 'sad'",
         |    "aPlusB": "a + b"
         |  }
         |}""".stripMargin

    val res: Opt[ExprSubstitutions] = toSubstitutions(inputJson)

    val expected = ExprSubstitutions(
      Map(
        ExpressionId("isHappy") -> IfElse(
          MatchRegex(FormCtx(FormComponentId("textBox1")), "[a-zA-Z0-9]{6}".r),
          Constant("happy"),
          Constant("sad")
        ),
        ExpressionId("aPlusB") -> Add(FormCtx(FormComponentId("a")), FormCtx(FormComponentId("b")))
      )
    )

    assertEquals(
      res.toString,
      Right(expected).toString
    ) // scala.util.matching.Regex in MatchRegex needs to be compared in String representation

  }

  test("ExprSubstitutions.from should fail when on invalid expressions") {
    val inputJson =
      """|{
         |  "expressions": {
         |    "isHappy": "if myChoice contains 1 then 'happy'"
         |  }
         |}""".stripMargin

    val res: Opt[ExprSubstitutions] = toSubstitutions(inputJson)

    val expected = Left(
      UnexpectedState(
        """|Unable to parse expression ${if myChoice contains 1 then 'happy'}.
           |Errors:
           |'else' expected but '}' found""".stripMargin
      )
    )

    assertEquals(res, expected)

  }

  test("ExprSubstitutions.from should fail when top-level 'expressions' field is not JsObject") {
    val inputJson =
      """|{
         |  "expressions": "string-not-an-object"
         |}""".stripMargin

    val res: Opt[ExprSubstitutions] = toSubstitutions(inputJson)

    val expected =
      Left(UnexpectedState("""Field 'expressions' needs to be a JsObject, but got JsString: "string-not-an-object""""))

    assertEquals(res, expected)
  }

  test("ExprSubstitutions.from should fail when expression is not JsString") {
    val inputJson =
      """|{
         |  "expressions": {
         |    "isHappy": 123,
         |    "aPlusB": "a + b"
         |  }
         |}""".stripMargin

    val res: Opt[ExprSubstitutions] = toSubstitutions(inputJson)

    val expected = Left(UnexpectedState("Wrong 'expressions', expected JsString, but got JsNumber: 123"))

    assertEquals(res, expected)
  }
}
