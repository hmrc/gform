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

import org.scalatest._
import scala.language.implicitConversions
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class BooleanExprParserSpec extends FlatSpec with Matchers with EitherValues with OptionValues {

  implicit def implicitToFormComponentId(str: String): FormComponentId = FormComponentId(str)

  "BooleanExprParser.validate" should "parse equality" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress=0}")

    res shouldBe Right(Equals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0")))

  }

  it should "parse inequality" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress!=0}")

    res shouldBe Right(Not(Equals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0"))))

  }

  it should "parse greater than" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress>0}")

    res shouldBe Right(GreaterThan(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0")))

  }

  it should "parse greater than or equal" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress>=0}")

    res shouldBe Right(GreaterThanOrEquals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0")))

  }

  it should "parse less than" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress<0}")

    res shouldBe Right(LessThan(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0")))

  }

  it should "parse less than or equal" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress<=0}")

    res shouldBe Right(LessThanOrEquals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0")))

  }

  "BooleanExprParser" should "parse expression with negative operand" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress<=-1}")

    res shouldBe Right(LessThanOrEquals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("-1")))
  }

  it should "parse expression with positive operand" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress<=+1}")

    res shouldBe Right(LessThanOrEquals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("+1")))
  }

  "BooleanExprParser" should "parse contains" in {
    val res = BooleanExprParser.validate("""${fieldId contains 1}""")

    res shouldBe Right(Contains(FormCtx("fieldId"), Constant("1")))

  }

  it should "parse logical negation expressions" in {
    val res = BooleanExprParser.validate("${!isPremisesSameAsBusinessAddress=0}")

    res shouldBe Right(Not(Equals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0"))))

  }

  it should "parse or-expressions" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress=0||amountA=22}")

    res shouldBe Right(
      Or(Equals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0")), Equals(FormCtx("amountA"), Constant("22"))))

  }

  it should "parse successive or-expressions" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress=0||amountA=22||amountB=33}")

    res shouldBe Right(
      Or(
        Or(
          Equals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0")),
          Equals(FormCtx("amountA"), Constant("22"))),
        Equals(FormCtx("amountB"), Constant("33"))
      )
    )

  }

  it should "parse and-expressions" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress=0&&amountA=22}")

    res shouldBe Right(
      And(
        Equals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0")),
        Equals(FormCtx("amountA"), Constant("22"))))

  }

  it should "parse successive and-expressions" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress=0&&amountA=22&&amountB=33}")

    res shouldBe Right(
      And(
        And(
          Equals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0")),
          Equals(FormCtx("amountA"), Constant("22"))),
        Equals(FormCtx("amountB"), Constant("33"))
      )
    )

  }

  it should "parse and-with-or-expressions using implicit priority" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress=0&&amountA=22||amountB=33}")

    res shouldBe Right(
      Or(
        And(
          Equals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0")),
          Equals(FormCtx("amountA"), Constant("22"))),
        Equals(FormCtx("amountB"), Constant("33"))
      )
    )

  }

  it should "parse and-with-or-expressions using implicit priority and opposite order" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress=0||amountA=22&&amountB=33}")

    res shouldBe Right(
      Or(
        Equals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0")),
        And(
          Equals(FormCtx("amountA"), Constant("22")),
          Equals(FormCtx("amountB"), Constant("33"))
        )
      )
    )

  }

  it should "parse and-with-or-expressions using explicit priority using brackets" in {
    val res = BooleanExprParser.validate("${(isPremisesSameAsBusinessAddress=0||amountA=22)&&amountB=33}")

    res shouldBe Right(
      And(
        Or(
          Equals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0")),
          Equals(FormCtx("amountA"), Constant("22"))),
        Equals(FormCtx("amountB"), Constant("33"))
      )
    )

  }

  it should "parse or-expressions including spaces and string constants" in {
    val res = BooleanExprParser.validate("${user.affinityGroup = 'organisation' || user.affinityGroup = 'individual'}")

    res shouldBe Right(
      Or(
        Equals(UserCtx(UserField.AffinityGroup), Constant("organisation")),
        Equals(UserCtx(UserField.AffinityGroup), Constant("individual"))))

  }

  it should "parse or-expressions inside form context" in {
    val res = BooleanExprParser.validate("${hasOrgsAddressChanged=1||hasOrgsAddressChanged=0}")

    res shouldBe Right(
      Or(
        Equals(FormCtx("hasOrgsAddressChanged"), Constant("1")),
        Equals(FormCtx("hasOrgsAddressChanged"), Constant("0"))))

  }

  def spacesBeforeCaret(message: String): Int =
    "[ ]+(?=\\^)".r.unanchored.findAllIn(message).toList.last.length

  it should "fail to parse anything but an equals operator" in {
    val res = BooleanExprParser.validate("${abc|=form.amountA}")

    res should be('left)

    res.left.value match {
      case UnexpectedState(msg) => spacesBeforeCaret(msg) shouldBe "${abc".length
      case _                    => fail("${expected an UnexpectedState}")
    }

  }

  it should "fail to parse eeitt.businessUserx = XYZ}" in {
    val res = BooleanExprParser.validate("${eeitt.businessUserx=XYZ}")

    res should be('left)

    res.left.value match {
      case UnexpectedState(msg) =>
        msg shouldBe """|Unable to parse expression ${eeitt.businessUserx=XYZ}.
                        |Errors:
                        |${eeitt.businessUserx=XYZ}:1: unexpected characters; expected 'count' or 'sum'
                        |${eeitt.businessUserx=XYZ}        ^
                        |""".stripMargin.trim
      case _ => fail("expected an UnexpectedState")
    }
  }

  it should "parse true" in {
    BooleanExprParser.validate("${true}") shouldBe Right(IsTrue)
  }

  it should "parse before - form context variables" in {
    val res = BooleanExprParser.validate("${startDate before endDate}")

    res shouldBe Right(DateBefore(DateFormCtxVar(FormCtx("startDate")), DateFormCtxVar(FormCtx("endDate"))))
  }

  it should "parse before - with braces" in {
    val res = BooleanExprParser.validate("${(startDate before endDate)}")
    res shouldBe Right(DateBefore(DateFormCtxVar(FormCtx("startDate")), DateFormCtxVar(FormCtx("endDate"))))
  }

  it should "parse before - TODAY" in {
    val res = BooleanExprParser.validate("${TODAY before TODAY}")

    res shouldBe Right(DateBefore(DateValueExpr(TodayDateExprValue), DateValueExpr(TodayDateExprValue)))
  }

  it should "parse before - exact value" in {
    val res = BooleanExprParser.validate("${01022020 before 01022021}")

    res shouldBe Right(
      DateBefore(DateValueExpr(ExactDateExprValue(2020, 2, 1)), DateValueExpr(ExactDateExprValue(2021, 2, 1))))
  }

  it should "parse before - TODAY and exact value" in {
    val res = BooleanExprParser.validate("${TODAY before '01022020'}")

    res shouldBe Right(DateBefore(DateValueExpr(TodayDateExprValue), DateValueExpr(ExactDateExprValue(2020, 2, 1))))
  }

  it should "parse before - TODAY and form context variable" in {
    val res = BooleanExprParser.validate("${TODAY before startDate}")

    res shouldBe Right(DateBefore(DateValueExpr(TodayDateExprValue), DateFormCtxVar(FormCtx("startDate"))))
  }

  it should "parse before - TODAY + offset" in {
    val res = BooleanExprParser.validate("${TODAY + 1d before startDate}")

    res shouldBe Right(
      DateBefore(
        DateExprWithOffset(DateValueExpr(TodayDateExprValue), 1, OffsetUnitDay),
        DateFormCtxVar(FormCtx("startDate"))))
  }

  it should "parse before - form context variable + offset" in {
    val res = BooleanExprParser.validate("${startDate + 1d before endDate}")

    res shouldBe Right(
      DateBefore(
        DateExprWithOffset(DateFormCtxVar(FormCtx("startDate")), 1, OffsetUnitDay),
        DateFormCtxVar(FormCtx("endDate"))))
  }

  it should "parse before - return error when offset is invalid" in {
    val res = BooleanExprParser.validate("${startDate *1d after endDate}")

    res shouldBe Left(
      UnexpectedState(
        """Unable to parse expression ${startDate *1d after endDate}.
          |Errors:
          |${startDate *1d after endDate}:1: unexpected characters; expected '+' or '=' or '\s+' or '<=' or '*' or '>=' or '!=' or '-' or 'else' or '>' or '<'
          |${startDate *1d after endDate}              ^""".stripMargin))
  }

  it should "parse before - should work with other expressions" in {
    val res = BooleanExprParser.validate("${startDate -1d after endDate && field1 = field2}")

    res shouldBe Right(
      And(
        DateAfter(
          DateExprWithOffset(DateFormCtxVar(FormCtx("startDate")), -1, OffsetUnitDay),
          DateFormCtxVar(FormCtx("endDate"))),
        Equals(FormCtx("field1"), FormCtx("field2"))
      ))
  }

  it should "parse after - form context variables" in {
    val res = BooleanExprParser.validate("${startDate after endDate}")

    res shouldBe Right(DateAfter(DateFormCtxVar(FormCtx("startDate")), DateFormCtxVar(FormCtx("endDate"))))
  }
}
