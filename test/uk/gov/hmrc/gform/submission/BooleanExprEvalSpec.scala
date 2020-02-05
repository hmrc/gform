/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.submission

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scala.language.implicitConversions
import uk.gov.hmrc.gform.sharedmodel.VariadicFormData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DataSource.SeissEligible
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import org.scalatest.prop.TableDrivenPropertyChecks

class BooleanExprEvalSpec extends FlatSpec with Matchers with EitherValues with TableDrivenPropertyChecks {

  implicit def implicitToFormComponentId(str: String): FormComponentId = FormComponentId(str)

  "isTrue" should "evaluate Equals with simple FormCtx fields" in {
    val data = VariadicFormData.ones(FormComponentId("firstName") -> "Pete")

    BooleanExprEval.isTrue(Equals(FormCtx("firstName"), Constant("Pete")), data, None).beResult shouldBe true
    BooleanExprEval.isTrue(Equals(FormCtx("firstName"), Constant("*Not*Pete")), data, None).beResult shouldBe false
  }

  it should "evaluate Equals with MultiField FormCtxs" in {
    val data = VariadicFormData.ones(FormComponentId("startDate.year") -> "2010")

    BooleanExprEval.isTrue(Equals(FormCtx("startDate.year"), Constant("2010")), data, None).beResult shouldBe true
    BooleanExprEval.isTrue(Equals(FormCtx("startDate.year"), Constant("2011")), data, None).beResult shouldBe false
  }

  it should "evaluate And" in {
    val data = VariadicFormData.ones(
      FormComponentId("startDate.year") -> "2010",
      FormComponentId("firstName")      -> "Pete"
    )

    val table = Table(
      ("lhs", "rhs", "expected"),
      (Equals(FormCtx("startDate.year"), Constant("2010")), Equals(FormCtx("firstName"), Constant("Pete")), true),
      (Equals(FormCtx("startDate.year"), Constant("2010")), Equals(FormCtx("firstName"), Constant("*Not*Pete")), false),
      (Equals(FormCtx("startDate.year"), Constant("2011")), Equals(FormCtx("firstName"), Constant("*Not*Pete")), false),
      (Equals(FormCtx("startDate.year"), Constant("2011")), Equals(FormCtx("firstName"), Constant("Pete")), false)
    )

    forAll(table) { (lhs: BooleanExpr, rhs: BooleanExpr, expected: Boolean) =>
      BooleanExprEval.isTrue(And(lhs, rhs), data, None).beResult shouldBe expected
    }
  }

  it should "evaluate Or" in {
    val data = VariadicFormData.ones(
      FormComponentId("startDate.year") -> "2010",
      FormComponentId("firstName")      -> "Pete"
    )

    val table = Table(
      ("lhs", "rhs", "expected"),
      (Equals(FormCtx("startDate.year"), Constant("2010")), Equals(FormCtx("firstName"), Constant("Pete")), true),
      (Equals(FormCtx("startDate.year"), Constant("2010")), Equals(FormCtx("firstName"), Constant("*Not*Pete")), true),
      (Equals(FormCtx("startDate.year"), Constant("2011")), Equals(FormCtx("firstName"), Constant("*Not*Pete")), false),
      (Equals(FormCtx("startDate.year"), Constant("2011")), Equals(FormCtx("firstName"), Constant("Pete")), true)
    )

    forAll(table) { (lhs: BooleanExpr, rhs: BooleanExpr, expected: Boolean) =>
      BooleanExprEval.isTrue(Or(lhs, rhs), data, None).beResult shouldBe expected
    }
  }

  it should "evaluate IsTrue" in {
    BooleanExprEval.isTrue(IsTrue, VariadicFormData.empty, None).beResult shouldBe true
  }

  it should "evaluate IsFalse" in {
    BooleanExprEval.isTrue(IsFalse, VariadicFormData.empty, None).beResult shouldBe false
  }

  it should "evaluate Includes" in {
    val data = VariadicFormData.many(FormComponentId("multichoice"), Seq("0", "2"))

    BooleanExprEval.isTrue(Contains(FormCtx("multichoice"), Constant("0")), data, None).beResult shouldBe true
    BooleanExprEval.isTrue(Contains(FormCtx("multichoice"), Constant("2")), data, None).beResult shouldBe true
    BooleanExprEval.isTrue(Contains(FormCtx("multichoice"), Constant("1")), data, None).beResult shouldBe false
  }

  it should "evaluate exits" in {
    val data = VariadicFormData.one(FormComponentId("singleField"), "1234567890")

    BooleanExprEval.isTrue(In(FormCtx("singleField"), SeissEligible), data, None).beResult shouldBe true
    BooleanExprEval.isTrue(In(FormCtx("noSingleField"), SeissEligible), data, None).beResult shouldBe false
  }
}
