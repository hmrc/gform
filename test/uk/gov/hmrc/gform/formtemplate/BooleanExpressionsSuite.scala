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
import play.api.libs.json.{ JsValue, Json }
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateRaw

class BooleanExpressionsSuite extends FunSuite {

  test("BooleanExpressionsSuite - self-referencing") {
    booleanExpressionsContextFromJson(
      Json.obj(
        "foo" -> "bar",
        "bar" -> "bar",
        "baz" -> "foo"
      )
    )
  }

  private def booleanExpressionsContextFromJson(json: JsValue): Unit = {
    val templateRaw = FormTemplateRaw(Json.obj("booleanExpressions" -> json))
    val booleanExpressionsContextOpt: Opt[BooleanExprSubstitutions] = BooleanExprSubstitutions.resolvedFrom(templateRaw)

    booleanExpressionsContextOpt match {
      case Left(node) =>
        assertEquals(
          node,
          UnexpectedState(
            "The booleanExpression bar cannot reference itself"
          )
        )
      case Right(booleanExprSubstitutions) => fail("Failed self-referencing detection")
    }
  }
}
