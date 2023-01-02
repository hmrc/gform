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

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ CsvColumnName, FormComponentId, FormCtx }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SelectionCriteriaValue.{ SelectionCriteriaExpr, SelectionCriteriaReference, SelectionCriteriaSimpleValue }

class SelectionCriteriaParserSpec extends Spec {

  val validate = SelectionCriteriaParser.validate _

  "FormCtx Expr" should "be parsed successfully" in {
    val res = validate("${travelMethod}")

    res.right.value should be(SelectionCriteriaExpr(FormCtx(FormComponentId("travelMethod"))))
  }

  "FormCtx Expr and csv column name" should "be parsed successfully" in {
    val res = validate("country.CountryCode")

    res.right.value should be(
      SelectionCriteriaReference(FormCtx(FormComponentId("country")), CsvColumnName("CountryCode"))
    )
  }

  "Numeric value" should "be parsed successfully" in {
    val res = validate("2")

    res.right.value should be(SelectionCriteriaSimpleValue(List("2")))
  }

  "String value" should "be parsed successfully" in {
    val res = validate("UK")

    res.right.value should be(SelectionCriteriaSimpleValue(List("UK")))
  }

  "FormCtx Expr and csv column name in invalid format" should "throw exception" in {
    val res = validate("${travelMethod}.CountryCode")

    res.left.value should be(UnexpectedState("""Unable to parse expression ${travelMethod}.CountryCode.
                                               |Errors:
                                               |end of input expected""".stripMargin))
  }

}
