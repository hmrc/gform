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

import org.scalatest._
import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class BooleanExprSpec extends FlatSpec with Matchers with EitherValues {

  val data = Map(
    FormComponentId("startDate.year")  -> "2010",
    FormComponentId("startDate.day")   -> "10",
    FormComponentId("startDate.month") -> "10",
    FormComponentId("firstName")       -> "Pete",
    FormComponentId("nameOfBusiness")  -> "Business Name"
  ).mapValues(_ :: Nil)

  "isTrue" should "evaluate correctly" in {

    BooleanExpr.isTrue(Equals(FormCtx("firstName"), Constant("Pete")), data, None).beResult shouldBe true
    BooleanExpr.isTrue(Equals(FormCtx("startDate.year"), Constant("2010")), data, None).beResult shouldBe true
    BooleanExpr
      .isTrue(
        And(Equals(FormCtx("startDate.year"), Constant("2010")), Equals(FormCtx("firstName"), Constant("Pete"))),
        data,
        None)
      .beResult shouldBe true
    BooleanExpr.isTrue(Equals(FormCtx("firstName"), Constant("*Not*Pete")), data, None).beResult shouldBe false
    BooleanExpr
      .isTrue(
        And(Equals(FormCtx("startDate.year"), Constant("2010")), Equals(FormCtx("firstName"), Constant("*Not*Pete"))),
        data,
        None)
      .beResult shouldBe false
    BooleanExpr
      .isTrue(
        Or(Equals(FormCtx("startDate.year"), Constant("2010")), Equals(FormCtx("firstName"), Constant("*Not*Pete"))),
        data,
        None)
      .beResult shouldBe true
    BooleanExpr.isTrue(IsTrue, Map(), None).beResult shouldBe true
    BooleanExpr.isTrue(IsFalse, Map(), None).beResult shouldBe false

  }

}
