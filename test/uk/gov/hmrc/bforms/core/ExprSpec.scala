/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.core

import org.scalatest._
import org.scalatest.matchers.{ BeMatcher, MatchResult, Matcher }
import play.api.libs.json.{ Json, JsString, JsResult, JsValue, Reads, Writes }

class ExprSpec extends FlatSpec with Matchers with JsResultMatcher {

  val add = Add(FormCtx("fieldA"), FormCtx("fieldB"))
  val addJson = Json.obj(
    "Add" -> Json.obj(
      "field1" -> Json.obj("FormCtx" -> Json.obj("value" -> "fieldA")),
      "field2" -> Json.obj("FormCtx" -> Json.obj("value" -> "fieldB"))
    )
  )

  "Expr" should "write Add case class to json" in {
    val res: JsValue = implicitly[Writes[Expr]].writes(add)
    res should be(addJson)
  }

  it should "read Add case class from json (coming from mongo)" in {
    val res: JsResult[Expr] = implicitly[Reads[Expr]].reads(addJson)
    res should beJsSuccess[Expr](add)
  }

  it should "read Add case class from expression (coming from template designer)" in {
    val res: JsResult[ValueExpr] = implicitly[Reads[ValueExpr]].reads(JsString("${fieldA + fieldB}"))
    res should beJsSuccess[ValueExpr](TextExpression(add))
  }

  val constant = Constant("constant")
  val constantJson = Json.obj(
    "Constant" -> Json.obj("value" -> "constant")
  )

  it should "write Constant to json" in {
    val res: JsValue = implicitly[Writes[Expr]].writes(constant)
    res should be(constantJson)
  }

  it should "read Constant from json (coming from mongo)" in {
    val res: JsResult[Expr] = implicitly[Reads[Expr]].reads(constantJson)
    res should beJsSuccess[Expr](constant)
  }

  it should "read Constant from string (coming from template designer)" in {
    val res: JsResult[ValueExpr] = implicitly[Reads[ValueExpr]].reads(JsString("constant"))
    res should beJsSuccess[ValueExpr](TextExpression(Constant("constant")))
  }

  val form = FormCtx("form")
  val formJson = Json.obj(
    "FormCtx" -> Json.obj("value" -> "form")
  )

  it should "write FormCtx to json" in {
    val res: JsValue = implicitly[Writes[Expr]].writes(form)
    res should be(formJson)
  }

  it should "read FormCtx from json" in {
    val res: JsResult[Expr] = implicitly[Reads[Expr]].reads(formJson)
    res should beJsSuccess[Expr](form)
  }

}

trait JsResultMatcher {

  /**
   * Checks to see if `play.api.libs.json.JsResult` is a specific JsSuccess element.
   */
  def beJsSuccess[E](element: E): Matcher[JsResult[E]] = new BeJsResult[E](element)

  /**
   * Checks to see if `play.api.libs.json.JsResult` is a `JsError`.
   */
  def jsError[E]: BeMatcher[JsResult[E]] = new IsJsErrorMatcher[E]

  final private class BeJsResult[E](element: E) extends Matcher[JsResult[E]] {
    def apply(jsResult: JsResult[E]): MatchResult = {
      MatchResult(
        jsResult.fold(_ => false, _ == element),
        s"'$jsResult' did not contain an element matching '$element'.",
        s"'$jsResult' contained an element matching '$element', but should not have."
      )
    }
  }

  final private class IsJsErrorMatcher[E] extends BeMatcher[JsResult[E]] {
    def apply(jsResult: JsResult[E]): MatchResult = MatchResult(
      jsResult.isError,
      s"'$jsResult' was not an JsError, but should have been.",
      s"'$jsResult' was an JsError, but should *NOT* have been."
    )
  }
}
