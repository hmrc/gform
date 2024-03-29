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
import uk.gov.hmrc.gform.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Register.Port
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SelectionCriteriaValue.SelectionCriteriaExpr

class FormatParserSpec extends Spec {

  val validate = FormatParser.validate(RoundingMode.defaultRoundingMode, None, EmailVerification.NoVerification) _

  val validateWithSelectionCriteria = FormatParser.validate(
    RoundingMode.defaultRoundingMode,
    Some(
      List(
        SelectionCriteria(CsvColumnName("PortType"), SelectionCriteriaExpr(FormCtx(FormComponentId("travelMethod"))))
      )
    ),
    EmailVerification.NoVerification
  ) _

  "shortText(1,35)" should "be parsed successfully " in {
    val res = validate("shortText(1,35)")
    res.toOption.value shouldBe TextFormat(ShortText(1, 35))
  }

  "lookup(originWho)" should "be parsed successfully " in {
    val res = validate("lookup(originWho)")
    res.toOption.value shouldBe TextFormat(Lookup(Register.OriginWho, None))
  }

  "lookup(intentBusiness)." should "be parsed successfully " in {
    val res = validate("lookup(intentBusiness)")
    res.toOption.value shouldBe TextFormat(Lookup(Register.IntentBusiness, None))
  }

  "YYY-MM-DD" should "be passed as it is" in {
    val res = validate("anyDate")
    res.toOption.value should be(DateFormat(AnyDate))
  }

  "after today -2" should "be parsed successfully" in {
    val res = validate("after today -2")
    res.toOption.value should be(DateFormat(DateConstraints(List(DateConstraint(After, Today, OffsetDate(-2))))))
  }

  "after 2017-04-02 -2" should "be parsed successfully" in {
    val res = validate("after 2017-04-02 -2")
    res.toOption.value should be(
      DateFormat(
        DateConstraints(
          List(DateConstraint(After, ConcreteDate(Year.Exact(2017), Month.Exact(4), Day.Exact(2)), OffsetDate(-2)))
        )
      )
    )
  }

  "after next-05-06 -2" should "be parsed successfully" ignore { //ignored until handled in gform-frontend
    val res = validate("after next-05-06 -2")
    res.toOption.value should be(
      DateFormat(
        DateConstraints(
          List(DateConstraint(After, ConcreteDate(Year.Next, Month.Exact(5), Day.Exact(6)), OffsetDate(-2)))
        )
      )
    )
  }

  "after ${otherField}" should "be parsed successfully" in {
    val res = validate("after ${otherField}")
    res.toOption.value should be(
      DateFormat(DateConstraints(List(DateConstraint(After, DateField(FormComponentId("otherField")), OffsetDate(0)))))
    )
  }

  "after previous-05-06 0" should "be parsed successfully" ignore { //ignored until handled in gform-frontend
    val res = validate("after previous-05-06 0")
    res.toOption.value should be(
      DateFormat(
        DateConstraints(
          List(DateConstraint(After, ConcreteDate(Year.Previous, Month.Exact(5), Day.Exact(6)), OffsetDate(0)))
        )
      )
    )
  }

  "before anyFieldId anotherWord 9" should "throw exception" in {
    val res = validate("before anyFieldId anotherWord 9")

    res.left.value should be(
      UnexpectedState(
        """Unable to parse expression before anyFieldId anotherWord 9.
          |Errors:
          |end of input expected""".stripMargin
      )
    )
  }

  "after 2016-6-9 9" should "throw exception" in {
    val res = validate("after 2016-6-9 9")

    res.left.value should be(
      UnexpectedState("""|Unable to parse expression after 2016-6-9 9.
                         |Errors:
                         |end of input expected""".stripMargin)
    )
  }

  "after YYYY-04-DD" should "be parsed successfully" in {
    val res = validate("after YYYY-04-DD")

    res.toOption.value should be(
      DateFormat(
        DateConstraints(List(DateConstraint(After, ConcreteDate(Year.Any, Month.Exact(4), Day.Any), OffsetDate(0))))
      )
    )

  }

  "before today -2" should "be parsed successfully" in {
    val res = validate("before today -2")
    res.toOption.value should be(DateFormat(DateConstraints(List(DateConstraint(Before, Today, OffsetDate(-2))))))
  }

  "before 2017-04-02 -2" should "be parsed successfully" in {
    val res = validate("before 2017-04-02 -2")
    res.toOption.value should be(
      DateFormat(
        DateConstraints(
          List(DateConstraint(Before, ConcreteDate(Year.Exact(2017), Month.Exact(4), Day.Exact(2)), OffsetDate(-2)))
        )
      )
    )
  }

  "before and after" should "be parsed successfully" in {
    val res = validate("before 2017-04-02 -2,after 2015-02-01 +42")
    res.toOption.value should be(
      DateFormat(
        DateConstraints(
          List(
            DateConstraint(Before, ConcreteDate(Year.Exact(2017), Month.Exact(4), Day.Exact(2)), OffsetDate(-2)),
            DateConstraint(After, ConcreteDate(Year.Exact(2015), Month.Exact(2), Day.Exact(1)), OffsetDate(42))
          )
        )
      )
    )
  }

  "before and after with first and last day" should "be parsed successfully" in {
    val res = validate("before 2017-04-firstDay -2, after 2015-02-lastDay +42")
    res.toOption.value should be(
      DateFormat(
        DateConstraints(
          List(
            DateConstraint(Before, ConcreteDate(Year.Exact(2017), Month.Exact(4), Day.First), OffsetDate(-2)),
            DateConstraint(After, ConcreteDate(Year.Exact(2015), Month.Exact(2), Day.Last), OffsetDate(42))
          )
        )
      )
    )
  }

  "precisely 2018-04-firstDay" should "be parsed successfully" in {
    val res = validate("precisely 2018-04-firstDay")
    res.toOption.value should be(
      DateFormat(
        DateConstraints(
          List(DateConstraint(Precisely, ConcreteDate(Year.Exact(2018), Month.Exact(4), Day.First), OffsetDate(0)))
        )
      )
    )
  }

  "precisely 2019-08-lastDay" should "be parsed successfully" in {
    val res = validate("precisely 2019-08-lastDay")
    res.toOption.value should be(
      DateFormat(
        DateConstraints(
          List(DateConstraint(Precisely, ConcreteDate(Year.Exact(2019), Month.Exact(8), Day.Last), OffsetDate(0)))
        )
      )
    )
  }

  "before 2019-08-lastDay" should "be parsed successfully" in {
    val res = validate("before 2019-08-lastDay")
    res.toOption.value should be(
      DateFormat(
        DateConstraints(
          List(DateConstraint(Before, ConcreteDate(Year.Exact(2019), Month.Exact(8), Day.Last), OffsetDate(0)))
        )
      )
    )
  }

  "before 2019-08-lastDay -2" should "be parsed successfully" in {
    val res = validate("before 2019-08-lastDay -2")
    res.toOption.value should be(
      DateFormat(
        DateConstraints(
          List(DateConstraint(Before, ConcreteDate(Year.Exact(2019), Month.Exact(8), Day.Last), OffsetDate(-2)))
        )
      )
    )
  }

  "precisely 2019-08-03" should "be parsed successfully" in {
    val res = validate("precisely 2019-08-03")
    res.toOption.value should be(
      DateFormat(
        DateConstraints(
          List(DateConstraint(Precisely, ConcreteDate(Year.Exact(2019), Month.Exact(8), Day.Exact(3)), OffsetDate(0)))
        )
      )
    )
  }

  "expressions without offset" should "be parsed successfully" in {
    val res = validate("before 2017-04-02,after 2017-02-01")
    res.toOption.value should be(
      DateFormat(
        DateConstraints(
          List(
            DateConstraint(Before, ConcreteDate(Year.Exact(2017), Month.Exact(4), Day.Exact(2)), OffsetDate(0)),
            DateConstraint(After, ConcreteDate(Year.Exact(2017), Month.Exact(2), Day.Exact(1)), OffsetDate(0))
          )
        )
      )
    )
  }

  "number" should "be parsed successfully" in {
    val res = validate("number")
    res.toOption.value should be(TextFormat(Number(11, 2, RoundingMode.defaultRoundingMode, None)))
  }

  "number(n,m)" should "be parsed successfully" in {
    val res = validate("number(3,4)")
    res.toOption.value should be(TextFormat(Number(3, 4, RoundingMode.defaultRoundingMode, None)))
  }

  "number(n,m,'u')" should "be parsed successfully" in {
    val res = validate("number(3,4,'u')")
    res.toOption.value should be(
      TextFormat(Number(3, 4, RoundingMode.defaultRoundingMode, Some(toLocalisedString("u"))))
    )
  }

  "positiveNumber" should "be parsed successfully" in {
    val res = validate("positiveNumber")
    res.toOption.value should be(TextFormat(PositiveNumber(11, 2, RoundingMode.defaultRoundingMode, None)))
  }

  "positiveNumber(n,m)" should "be parsed successfully" in {
    val res = validate("positiveNumber(3,4)")
    res.toOption.value should be(TextFormat(PositiveNumber(3, 4, RoundingMode.defaultRoundingMode, None)))
  }

  "positiveNumber(n,m,'u')" should "be parsed successfully" in {
    val res = validate("positiveNumber(3,4,'u')")
    res.toOption.value should be(
      TextFormat(PositiveNumber(3, 4, RoundingMode.defaultRoundingMode, Some(toLocalisedString("u"))))
    )
  }

  "positiveWholeNumber" should "be parsed successfully" in {
    val res = validate("positiveWholeNumber")
    res.toOption.value should be(TextFormat(PositiveNumber(11, 0, RoundingMode.defaultRoundingMode, None)))
  }

  "Lookup without SelectionCriteria" should "be parsed successfully" in {
    val res = validate("lookup(port)")
    res.toOption.value should be(TextFormat(Lookup(Port, None)))
  }

  "Lookup with SelectionCriteria" should "be parsed successfully" in {
    val res = validateWithSelectionCriteria("lookup(port)")
    res.toOption.value should be(
      TextFormat(
        Lookup(
          Port,
          Some(
            List(
              SelectionCriteria(
                CsvColumnName("PortType"),
                SelectionCriteriaExpr(FormCtx(FormComponentId("travelMethod")))
              )
            )
          )
        )
      )
    )
  }

}
