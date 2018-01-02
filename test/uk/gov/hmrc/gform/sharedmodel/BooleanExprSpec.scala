/*
 * Copyright 2018 HM Revenue & Customs
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
    FormComponentId("startDate.year") -> FormField(FormComponentId("startDate.year"), "2010"),
    FormComponentId("startDate.day") -> FormField(FormComponentId("startDate.day"), "10"),
    FormComponentId("startDate.month") -> FormField(FormComponentId("startDate.month"), "10"),
    FormComponentId("firstName") -> FormField(FormComponentId("firstName"), "Pete"),
    FormComponentId("nameOfBusiness") -> FormField(FormComponentId("nameOfBusiness"), "Business Name"))

  "isTrue" should "evaluate correctly" in {

    BooleanExpr.isTrue(Equals(FormCtx("firstName"), Constant("Pete")), data) shouldBe true
    BooleanExpr.isTrue(Equals(FormCtx("startDate.year"), Constant("2010")), data) shouldBe true
    BooleanExpr.isTrue(And(Equals(FormCtx("startDate.year"), Constant("2010")), Equals(FormCtx("firstName"), Constant("Pete"))), data) shouldBe true
    BooleanExpr.isTrue(Equals(FormCtx("firstName"), Constant("*Not*Pete")), data) shouldBe false
    BooleanExpr.isTrue(And(Equals(FormCtx("startDate.year"), Constant("2010")), Equals(FormCtx("firstName"), Constant("*Not*Pete"))), data) shouldBe false
    BooleanExpr.isTrue(Or(Equals(FormCtx("startDate.year"), Constant("2010")), Equals(FormCtx("firstName"), Constant("*Not*Pete"))), data) shouldBe true
    BooleanExpr.isTrue(IsTrue, Map()) shouldBe true
    BooleanExpr.isTrue(IsFalse, Map()) shouldBe false

  }

}
