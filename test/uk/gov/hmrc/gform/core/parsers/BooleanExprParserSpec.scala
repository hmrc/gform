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

import org.scalatest.{ EitherValues, OptionValues }
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formtemplate.BooleanExprId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.AddressDetail.Country
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class BooleanExprParserSpec extends AnyFlatSpec with Matchers with EitherValues with OptionValues {

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
      Or(Equals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0")), Equals(FormCtx("amountA"), Constant("22")))
    )

  }

  it should "parse successive or-expressions" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress=0||amountA=22||amountB=33}")

    res shouldBe Right(
      Or(
        Or(
          Equals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0")),
          Equals(FormCtx("amountA"), Constant("22"))
        ),
        Equals(FormCtx("amountB"), Constant("33"))
      )
    )

  }

  it should "parse and-expressions" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress=0&&amountA=22}")

    res shouldBe Right(
      And(Equals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0")), Equals(FormCtx("amountA"), Constant("22")))
    )

  }

  it should "parse successive and-expressions" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress=0&&amountA=22&&amountB=33}")

    res shouldBe Right(
      And(
        And(
          Equals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0")),
          Equals(FormCtx("amountA"), Constant("22"))
        ),
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
          Equals(FormCtx("amountA"), Constant("22"))
        ),
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
          Equals(FormCtx("amountA"), Constant("22"))
        ),
        Equals(FormCtx("amountB"), Constant("33"))
      )
    )

  }

  it should "parse or-expressions including spaces and string constants" in {
    val res = BooleanExprParser.validate("${user.affinityGroup = 'organisation' || user.affinityGroup = 'individual'}")

    res shouldBe Right(
      Or(
        Equals(UserCtx(UserField.AffinityGroup), Constant("organisation")),
        Equals(UserCtx(UserField.AffinityGroup), Constant("individual"))
      )
    )

  }

  it should "parse or-expressions inside form context" in {
    val res = BooleanExprParser.validate("${hasOrgsAddressChanged=1||hasOrgsAddressChanged=0}")

    res shouldBe Right(
      Or(
        Equals(FormCtx("hasOrgsAddressChanged"), Constant("1")),
        Equals(FormCtx("hasOrgsAddressChanged"), Constant("0"))
      )
    )

  }

  def spacesBeforeCaret(message: String): Int =
    "[ ]+(?=\\^)".r.unanchored.findAllIn(message).toList.last.length

  it should "fail to parse anything but an equals operator" in {
    val res = BooleanExprParser.validate("${abc|=form.amountA}")
    res should be(Symbol("left"))
    res.left.value shouldBe UnexpectedState("""Unable to parse expression ${abc|=form.amountA}.
                                              |Errors:
                                              |'}' expected but '|' found""".stripMargin)

  }

  it should "fail to parse eeitt.businessUserx = XYZ}" in {
    val res = BooleanExprParser.validate("${eeitt.businessUserx=XYZ}")

    res should be(Symbol("left"))

    res.left.value match {
      case UnexpectedState(msg) =>
        msg shouldBe ("""Unable to parse expression ${eeitt.businessUserx=XYZ}.
                         |Errors:
                         |'}' expected but '.' found""".stripMargin)
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
      DateBefore(DateValueExpr(ExactDateExprValue(2020, 2, 1)), DateValueExpr(ExactDateExprValue(2021, 2, 1)))
    )
  }

  it should "parse before - TODAY and exact value" in {
    val res = BooleanExprParser.validate("${TODAY before '01022020'}")

    res shouldBe Right(DateBefore(DateValueExpr(TodayDateExprValue), DateValueExpr(ExactDateExprValue(2020, 2, 1))))
  }

  it should "parse before - TODAY and form context variable" in {
    val res = BooleanExprParser.validate("${TODAY before startDate}")

    res shouldBe Right(DateBefore(DateValueExpr(TodayDateExprValue), DateFormCtxVar(FormCtx("startDate"))))
  }

  it should "parser today + 1d " in {

    val res = ValueParser.validateWithParser("TODAY + 1d", ValueParser.dateExpr)

    res shouldBe Right(
      DateExprWithOffset(DateValueExpr(TodayDateExprValue), OffsetYMD(OffsetUnit.Day(1)))
    )
  }

  it should "parse before - TODAY + offset" in {
    val res = BooleanExprParser.validate("${TODAY + 1d before startDate}")

    res shouldBe Right(
      DateBefore(
        DateExprWithOffset(DateValueExpr(TodayDateExprValue), OffsetYMD(OffsetUnit.Day(1))),
        DateFormCtxVar(FormCtx("startDate"))
      )
    )
  }

  it should "parse date expression" in {

    val res = ValueParser.validateWithParser("startDate + 1d", ValueParser.dateExpr)

    res shouldBe Right(
      DateExprWithOffset(DateFormCtxVar(FormCtx("startDate")), OffsetYMD(OffsetUnit.Day(1)))
    )
  }

  it should "parse before - form context variable + offset" in {
    val res = BooleanExprParser.validate("${startDate + 1d before endDate}")

    res shouldBe Right(
      DateBefore(
        DateExprWithOffset(DateFormCtxVar(FormCtx("startDate")), OffsetYMD(OffsetUnit.Day(1))),
        DateFormCtxVar(FormCtx("endDate"))
      )
    )
  }

  it should "parse before - return error when offset is invalid" in {
    val res = BooleanExprParser.validate("${startDate *1d after endDate}")

    res shouldBe Left(
      UnexpectedState(
        """Unable to parse expression ${startDate *1d after endDate}.
          |Errors:
          |'}' expected but '*' found""".stripMargin
      )
    )
  }

  it should "parse before - should work with other expressions" in {
    val res = BooleanExprParser.validate("${startDate -1d after endDate && field1 = field2}")

    res shouldBe Right(
      And(
        DateAfter(
          DateExprWithOffset(DateFormCtxVar(FormCtx("startDate")), OffsetYMD(OffsetUnit.Day(-1))),
          DateFormCtxVar(FormCtx("endDate"))
        ),
        Equals(FormCtx("field1"), FormCtx("field2"))
      )
    )
  }

  it should "parse after - form context variables" in {
    val res = BooleanExprParser.validate("${startDate after endDate}")

    res shouldBe Right(DateAfter(DateFormCtxVar(FormCtx("startDate")), DateFormCtxVar(FormCtx("endDate"))))
  }

  it should "parse expression ${overseasAddress.country != 'United Kingdom'} from template-NETP" in {
    val res = BooleanExprParser.validate("${overseasAddress.country != 'United Kingdom'}")
    res.toOption.value shouldBe Not(
      Equals(AddressLens(FormComponentId("overseasAddress"), Country), Constant("United Kingdom"))
    )
  }

  it should "parse single variable with prefix 'no' as boolean expression" in {
    val res = BooleanExprParser.validate("${noCompanyName}")
    res.toOption.value shouldBe TopLevelRef(BooleanExprId("noCompanyName"))
  }

  it should "parse after with HmrcTaxPeriodCtx" in {
    val res = BooleanExprParser.validate("${returnPeriod.periodTo after endDate}")

    res shouldBe Right(
      DateAfter(
        HmrcTaxPeriodCtx(FormCtx("returnPeriod"), HmrcTaxPeriodInfo.PeriodTo),
        DateFormCtxVar(FormCtx("endDate"))
      )
    )
  }

  it should "parse expression ${addToList.first}" in {
    val res = BooleanExprParser.validate("${addToList.first}")
    res.toOption.value shouldBe First(FormCtx(FormComponentId("addToList")))
  }

  it should "parse expression ${auth.emailLogin}" in {
    val res = BooleanExprParser.validate("${auth.emailLogin}")
    res.toOption.value shouldBe IsLogin(LoginInfo.EmailLogin)
  }

  it should "parse expression ${auth.ggLogin}" in {
    val res = BooleanExprParser.validate("${auth.ggLogin}")
    res.toOption.value shouldBe IsLogin(LoginInfo.GGLogin)
  }

  it should "parse an expression that includes substring" in {
    val res = BooleanExprParser.validate("${substring('test string',0,2) = 'XI'}")
    res.toOption.value shouldBe Equals(StringOps(Constant("test string"), StringFnc.SubString(0, 2)), Constant("XI"))
  }

  it should "parse an expression that includes substring with user field enrolments" in {
    val res = BooleanExprParser.validate("${substring(user.enrolments.HMRC-CUS-ORG.EORINumber,0,2) = 'XI'}")
    res.toOption.value shouldBe Equals(
      StringOps(
        UserCtx(UserField.Enrolment(ServiceName("HMRC-CUS-ORG"), IdentifierName("EORINumber"), None)),
        StringFnc.SubString(0, 2)
      ),
      Constant("XI")
    )
  }
}
