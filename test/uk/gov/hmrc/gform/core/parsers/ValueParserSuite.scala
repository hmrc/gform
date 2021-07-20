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

package uk.gov.hmrc.gform.core.parsers

import munit.FunSuite
import scala.language.implicitConversions
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
      "if a < b then foo else bar else baz",
      IfElse(LessThan("a", "b"), Else("foo", "bar"), "baz")
    ),
    (
      "if a < b then 'foo' else 'bar' else 'baz'",
      IfElse(LessThan("a", "b"), Else(Constant("foo"), Constant("bar")), Constant("baz"))
    )
  )

  table.zipWithIndex.foreach { case ((ifElse, expected), rowIndex) =>
    test(1 + rowIndex + ". " + ifElse) {

      val res = ValueParser.validate("${" + ifElse + "}")

      assertEquals(res, Right(TextExpression(expected)))

    }
  }
}
