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

package uk.gov.hmrc.gform.core.parsers

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formtemplate._
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class FormatParserSpec extends Spec {

  val validate = FormatParser.validate(RoundingMode.defaultRoundingMode) _

  "YYY-MM-DD" should "be passed as it is" in {
    val res = validate("anyDate")
    res.right.value should be(DateFormat(AnyDate))
  }

  "after today -2" should "be parsed successfully" in {
    val res = validate("after today -2")
    res.right.value should be(DateFormat(DateConstraints(List(DateConstraint(After, Today, OffsetDate(-2))))))
  }

  "after 2017-04-02 -2" should "be parsed successfully" in {
    val res = validate("after 2017-04-02 -2")
    res.right.value should be(
      DateFormat(DateConstraints(
        List(DateConstraint(After, ConcreteDate(ExactYear(2017), ExactMonth(4), ExactDay(2)), OffsetDate(-2))))))
  }

  "after next-05-06 -2" should "be parsed successfully" ignore { //ignored until handled in gform-frontend
    val res = validate("after next-05-06 -2")
    res.right.value should be(
      DateFormat(DateConstraints(List(DateConstraint(After, NextDate(ExactMonth(5), ExactDay(6)), OffsetDate(-2))))))
  }

  "after ${otherField}" should "be parsed successfully" in {
    val res = validate("after ${otherField}")
    res.right.value should be(
      DateFormat(DateConstraints(List(DateConstraint(After, DateField(FormComponentId("otherField")), OffsetDate(0))))))
  }

  "after previous-05-06 0" should "be parsed successfully" ignore { //ignored until handled in gform-frontend
    val res = validate("after previous-05-06 0")
    res.right.value should be(
      DateFormat(DateConstraints(List(DateConstraint(After, PreviousDate(ExactMonth(5), ExactDay(6)), OffsetDate(0))))))
  }

  "before anyFieldId anotherWord 9" should "throw exception" in {
    val res = validate("before anyFieldId anotherWord 9")

    res.left.value should be(
      UnexpectedState(
        """Unable to parse expression before anyFieldId anotherWord 9.
          |Errors:
          |before anyFieldId anotherWord 9:1: unexpected characters; expected '${' or 'previous' or 'next' or '(19|20)\d\d' or 'today'
          |before anyFieldId anotherWord 9       ^""".stripMargin))
  }

  "after 2016-6-9 9" should "throw exception" in {
    val res = validate("after 2016-6-9 9")

    res.left.value should be(
      UnexpectedState("""|Unable to parse expression after 2016-6-9 9.
                         |Errors:
                         |after 2016-6-9 9:1: unexpected characters; expected '0[1-9]|1[012]' or '\s+'
                         |after 2016-6-9 9           ^""".stripMargin))
  }

  "after YYYY-04-DD" should "throw exception" in {
    val res = validate("after YYYY-04-DD")

    res.left.value should be(
      UnexpectedState(
        """|Unable to parse expression after YYYY-04-DD.
           |Errors:
           |after YYYY-04-DD:1: unexpected characters; expected '${' or 'previous' or 'next' or '(19|20)\d\d' or 'today'
           |after YYYY-04-DD      ^""".stripMargin))
  }

  "before today -2" should "be parsed successfully" in {
    val res = validate("before today -2")
    res.right.value should be(DateFormat(DateConstraints(List(DateConstraint(Before, Today, OffsetDate(-2))))))
  }

  "before 2017-04-02 -2" should "be parsed successfully" in {
    val res = validate("before 2017-04-02 -2")
    res.right.value should be(
      DateFormat(DateConstraints(
        List(DateConstraint(Before, ConcreteDate(ExactYear(2017), ExactMonth(4), ExactDay(2)), OffsetDate(-2))))))
  }

  "before and after" should "be parsed successfully" in {
    val res = validate("before 2017-04-02 -2,after 2015-02-01 +42")
    res.right.value should be(
      DateFormat(DateConstraints(List(
        DateConstraint(Before, ConcreteDate(ExactYear(2017), ExactMonth(4), ExactDay(2)), OffsetDate(-2)),
        DateConstraint(After, ConcreteDate(ExactYear(2015), ExactMonth(2), ExactDay(1)), OffsetDate(42))
      ))))
  }

  "precisely 2018-04-firstDay" should "be parsed successfully" in {
    val res = validate("precisely 2018-04-firstDay")
    res.right.value should be(
      DateFormat(DateConstraints(
        List(DateConstraint(Precisely, ConcreteDate(ExactYear(2018), ExactMonth(4), FirstDay), OffsetDate(0))))))
  }

  "precisely 2019-08-lastDay" should "be parsed successfully" in {
    val res = validate("precisely 2019-08-lastDay")
    res.right.value should be(
      DateFormat(DateConstraints(
        List(DateConstraint(Precisely, ConcreteDate(ExactYear(2019), ExactMonth(8), LastDay), OffsetDate(0))))))
  }

  "before 2019-08-lastDay" should "be parsed successfully" in {
    val res = validate("before 2019-08-lastDay")
    println(res)
    res.right.value should be(
      DateFormat(DateConstraints(
        List(DateConstraint(Before, ConcreteDate(ExactYear(2019), ExactMonth(8), LastDay), OffsetDate(0))))))
  }

  "before 2019-08-lastDay -2" should "be parsed successfully" in {
    val res = validate("before 2019-08-lastDay -2")
    res.right.value should be(
      DateFormat(DateConstraints(
        List(DateConstraint(Before, ConcreteDate(ExactYear(2019), ExactMonth(8), LastDay), OffsetDate(-2))))))
  }

  "precisely 2019-08-03" should "be parsed successfully" in {
    val res = validate("precisely 2019-08-03")
    res.right.value should be(
      DateFormat(DateConstraints(
        List(DateConstraint(Precisely, ConcreteDate(ExactYear(2019), ExactMonth(8), ExactDay(3)), OffsetDate(0))))))
  }

  "expressions without offset" should "be parsed successfully" in {
    val res = validate("before 2017-04-02,after 2017-02-01")
    res.right.value should be(
      DateFormat(DateConstraints(List(
        DateConstraint(Before, ConcreteDate(ExactYear(2017), ExactMonth(4), ExactDay(2)), OffsetDate(0)),
        DateConstraint(After, ConcreteDate(ExactYear(2017), ExactMonth(2), ExactDay(1)), OffsetDate(0))
      ))))
  }

  "number" should "be parsed successfully" in {
    val res = validate("number")
    res.right.value should be(TextFormat(Number(11, 2, RoundingMode.defaultRoundingMode, None)))
  }

  "number(n,m)" should "be parsed successfully" in {
    val res = validate("number(3,4)")
    res.right.value should be(TextFormat(Number(3, 4, RoundingMode.defaultRoundingMode, None)))
  }

  "number(n,m,'u')" should "be parsed successfully" in {
    val res = validate("number(3,4,'u')")
    res.right.value should be(TextFormat(Number(3, 4, RoundingMode.defaultRoundingMode, Some("u"))))
  }

  "positiveNumber" should "be parsed successfully" in {
    val res = validate("positiveNumber")
    res.right.value should be(TextFormat(PositiveNumber(11, 2, RoundingMode.defaultRoundingMode, None)))
  }

  "positiveNumber(n,m)" should "be parsed successfully" in {
    val res = validate("positiveNumber(3,4)")
    res.right.value should be(TextFormat(PositiveNumber(3, 4, RoundingMode.defaultRoundingMode, None)))
  }

  "positiveNumber(n,m,'u')" should "be parsed successfully" in {
    val res = validate("positiveNumber(3,4,'u')")
    res.right.value should be(TextFormat(PositiveNumber(3, 4, RoundingMode.defaultRoundingMode, Some("u"))))
  }

  "positiveWholeNumber" should "be parsed successfully" in {
    val res = validate("positiveWholeNumber")
    res.right.value should be(TextFormat(PositiveNumber(11, 0, RoundingMode.defaultRoundingMode, None)))
  }
}
