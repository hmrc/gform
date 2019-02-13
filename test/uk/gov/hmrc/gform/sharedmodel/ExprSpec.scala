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

import play.api.libs.json._
import uk.gov.hmrc.gform._
import uk.gov.hmrc.gform.core.parsers.ExprParsers
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.util.Right

class ExprSpec extends Spec {

  private val add = Add(FormCtx("fieldA"), FormCtx("fieldB"))
  private val addJson = Json.obj(
    "Add" -> Json.obj(
      "field1" -> Json.obj("FormCtx" -> Json.obj("value" -> "fieldA")),
      "field2" -> Json.obj("FormCtx" -> Json.obj("value" -> "fieldB"))))

  "Expr" should "write Add case class to json" in {
    val res: JsValue = implicitly[Writes[Expr]].writes(add)
    res should be(addJson)
  }

  it should "read Add case class from json (coming from mongo)" in {
    val res: JsResult[Expr] = implicitly[Reads[Expr]].reads(addJson)
    res should beJsSuccess[Expr](add)
  }

  private val constant = Constant("constant")
  private val constantJson = Json.obj("Constant" -> Json.obj("value" -> "constant"))

  it should "write Constant to json" in {
    val res: JsValue = implicitly[Writes[Expr]].writes(constant)
    res should be(constantJson)
  }

  it should "read Constant from json (coming from mongo)" in {
    val res: JsResult[Expr] = implicitly[Reads[Expr]].reads(constantJson)
    res should beJsSuccess[Expr](constant)
  }

  private val formCtx = FormCtx("form")
  private val formJson = Json.obj("FormCtx" -> Json.obj("value" -> "form"))

  it should "write FormCtx to json" in {
    val res: JsValue = implicitly[Writes[Expr]].writes(formCtx)
    res should be(formJson)
  }

  it should "read FormCtx from json" in {
    val res: JsResult[Expr] = implicitly[Reads[Expr]].reads(formJson)
    res should beJsSuccess[Expr](formCtx)
  }

  it should "successfully parse a string with a string encapsulated by a '${' and a '}'  " in {
    val input = "${textBox}"
    val expected = "textBox"

    val res = ExprParsers.validateFormCtx(input)
    println(res)
    res shouldBe Right(FormCtx(expected))
  }
}
