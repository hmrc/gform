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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class DateExprSpec extends AnyFlatSpecLike with Matchers {

  "leafExprs" should "return all leaf values for DateValueExpr" in {
    val leafs = DateValueExpr(TodayDateExprValue).leafExprs
    leafs shouldBe List(DateCtx(DateValueExpr(TodayDateExprValue)))
  }

  it should "return all leaf values for DateFormCtxVar" in {
    val leafs = DateFormCtxVar(FormCtx(FormComponentId("someId"))).leafExprs
    leafs shouldBe List(FormCtx(FormComponentId("someId")))
  }

  it should "return all leaf values for DateExprWithOffset" in {
    val leafs =
      DateExprWithOffset(DateFormCtxVar(FormCtx(FormComponentId("someId"))), OffsetYMD(OffsetUnit.Day(1))).leafExprs
    leafs shouldBe List(FormCtx(FormComponentId("someId")))
  }

  it should "return all leaf values for HmrcTaxPeriodCtx" in {
    val leafs =
      HmrcTaxPeriodCtx(FormCtx(FormComponentId("someId")), HmrcTaxPeriodInfo.PeriodTo).leafExprs
    leafs shouldBe List(FormCtx(FormComponentId("someId")))
  }
}
