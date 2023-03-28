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

package uk.gov.hmrc.gform.core.parsers

import munit.FunSuite
import play.api.libs.json.{ JsString, JsSuccess, Reads }
import scala.language.implicitConversions
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class ValueParserSuite extends FunSuite {

  implicit def implicitToFormComponentId(str: String): FormComponentId = FormComponentId(str)

  implicit def liftToFormCtx(s: String): FormCtx = FormCtx(s)

  val table: List[(String, Expr)] = List(
    (
      "if a < b then foo else bar",
      IfElse(LessThan("a", "b"), "foo", "bar")
    ),
    (
      "if a < b then 'foo' else 'bar'",
      IfElse(LessThan("a", "b"), Constant("foo"), Constant("bar"))
    ),
    (
      "if a < b then foo orElse bar else baz",
      IfElse(LessThan("a", "b"), Else("foo", "bar"), "baz")
    ),
    (
      "if a < b then 'foo' orElse 'bar' else 'baz'",
      IfElse(LessThan("a", "b"), Else(Constant("foo"), Constant("bar")), Constant("baz"))
    ),
    (
      """|if ownerFc.count=1 then
         |    if form.lang='en' then 'Director' else 'cy-Director'
         |else
         |    if form.lang='en' then 'Directors' else 'cy-Directors'""".stripMargin,
      IfElse(
        Equals(Count("ownerFc"), Constant("1")),
        IfElse(Equals(LangCtx, Constant("en")), Constant("Director"), Constant("cy-Director")),
        IfElse(Equals(LangCtx, Constant("en")), Constant("Directors"), Constant("cy-Directors"))
      )
    )
  )

  table.zipWithIndex.foreach { case ((ifElse, expected), rowIndex) =>
    test(s"1 + $rowIndex .$ifElse") {

      val res = ValueParser.validate("${" + ifElse + "}")

      assertEquals(res, Right(TextExpression(expected)))

    }
  }

  val tableWithRegex: List[(String, Expr)] = List(
    (
      "if textBox1 match '^[a-zA-Z0-9]{5}' then 'foo' else 'bar'",
      IfElse(MatchRegex("textBox1", "^[a-zA-Z0-9]{5}".r), Constant("foo"), Constant("bar"))
    )
  )

  tableWithRegex.zipWithIndex.foreach { case ((ifElse, expected), rowIndex) =>
    test(s"1 $rowIndex . $ifElse (string comparison)") {

      val res = ValueParser.validate("${" + ifElse + "}")

      assertEquals(res.toString, Right(TextExpression(expected)).toString)
    }
  }

  val smartStringWithIfElse = List(
    (
      "Hello ${if a < b then 'foo' else 'bar'} world",
      "Hello {0} world",
      List(
        IfElse(LessThan("a", "b"), Constant("foo"), Constant("bar"))
      )
    )
    // This will fail to parse since wrong '}' will be detected to end the expression.
    /* (
     *   "Hello ${if a match '[a-b]{5}' then 'foo' else 'bar'} world",
     *   "Hello {0} world",
     *   List(
     *     IfElse(MatchRegex("a", "[a-b]{5}".r), Constant("foo"), Constant("bar"))
     *   )
     * ) */
  )

  smartStringWithIfElse.foreach { case (input, expectedString, expectedExpressions) =>
    test("SmartSgtring with " + input) {
      val result = implicitly[Reads[SmartString]].reads(JsString(input))
      assertEquals(
        result,
        JsSuccess(SmartString(LocalisedString(Map(LangADT.En -> expectedString)), expectedExpressions))
      )
    }
  }

  test("Localised String should expand to if-else expression") {
    val full = ValueParser.validate("${if form.lang = 'en' then 'EN' else 'CY'}")
    val compact = ValueParser.validate("${'EN'|'CY'}")

    assertEquals(full, compact)
  }

  test("parse expression ${auth.itmpName}") {
    val res = ValueParser.validate("${auth.itmpName}")
    res.toOption.value shouldBe TextExpression(AuthCtx(AuthInfo.ItmpName))
  }

  test("parse expression ${auth.itmpName.givenName}") {
    val res = ValueParser.validate("${auth.itmpName.givenName}")
    res.toOption.value shouldBe TextExpression(AuthCtx(AuthInfo.ItmpNameLens(ItmpNameFocus.GivenName)))
  }

  test("parse expression ${auth.itmpName.middleName}") {
    val res = ValueParser.validate("${auth.itmpName.middleName}")
    res.toOption.value shouldBe TextExpression(AuthCtx(AuthInfo.ItmpNameLens(ItmpNameFocus.MiddleName)))
  }

  test("parse expression ${auth.itmpName.familyName}") {
    val res = ValueParser.validate("${auth.itmpName.familyName}")
    res.toOption.value shouldBe TextExpression(AuthCtx(AuthInfo.ItmpNameLens(ItmpNameFocus.FamilyName)))
  }

}
