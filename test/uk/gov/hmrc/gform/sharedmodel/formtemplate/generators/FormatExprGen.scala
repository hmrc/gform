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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ After, AnyDate, AnyDay, AnyMonth, AnyText, AnyYear, BasicText, Before, BeforeAfterPrecisely, CompanyRegistrationNumber, ConcreteDate, CountryCode, DateConstraint, DateConstraintInfo, DateConstraintType, DateConstraints, DateField, EORI, Email, ExactDay, ExactMonth, ExactYear, FirstDay, LastDay, NINO, Next, NonUkCountryCode, Number, OffsetDate, PositiveNumber, Precisely, Previous, RoundingMode, ShortText, Sterling, TelephoneNumber, TextConstraint, TextExpression, TextWithRestrictions, Today, UTR, UkBankAccountNumber, UkSortCodeFormat, UkVrn }

trait FormatExprGen {
  def numberGen: Gen[Number] =
    for {
      maxWholeDigits      <- Gen.posNum[Int]
      maxFractionalDigits <- Gen.posNum[Int]
      units               <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      roundingMode        <- roundingModeGen
    } yield Number(maxWholeDigits, maxFractionalDigits, roundingMode, units)

  def positiveNumberGen: Gen[PositiveNumber] =
    for {
      maxWholeDigits      <- Gen.posNum[Int]
      maxFractionalDigits <- Gen.posNum[Int]
      units               <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      roundingMode        <- roundingModeGen
    } yield PositiveNumber(maxWholeDigits, maxFractionalDigits, roundingMode, units)

  def roundingModeGen: Gen[RoundingMode] =
    for {
      roundingMode <- Gen.oneOf(
                       RoundingMode.Up,
                       RoundingMode.Down,
                       RoundingMode.Floor,
                       RoundingMode.Ceiling,
                       RoundingMode.HalfEven,
                       RoundingMode.HalfDown,
                       RoundingMode.HalfUp
                     )
    } yield roundingMode

  def textWithRestrictions: Gen[TextWithRestrictions] =
    for {
      min <- Gen.posNum[Int]
      max <- Gen.posNum[Int]
    } yield TextWithRestrictions(min, max)

  def textConstraintGen: Gen[TextConstraint] = Gen.oneOf(
    Gen.const(AnyText),
    numberGen,
    positiveNumberGen,
    Gen.const(BasicText),
    Gen.const(ShortText),
    textWithRestrictions,
    Gen.const(Sterling(RoundingMode.defaultRoundingMode)),
    Gen.const(UkBankAccountNumber),
    Gen.const(UkSortCodeFormat),
    Gen.const(UTR),
    Gen.const(NINO),
    Gen.const(TelephoneNumber),
    Gen.const(Email),
    Gen.const(UkVrn),
    Gen.const(CountryCode),
    Gen.const(NonUkCountryCode),
    Gen.const(CompanyRegistrationNumber),
    Gen.const(EORI)
  )

  def textExpressionGen: Gen[TextExpression] = ExprGen.exprGen().map(TextExpression(_))

  def beforeOrAfterOrPreciselyGen: Gen[BeforeAfterPrecisely] = Gen.oneOf(Before, After, Precisely)

  def exactYearGen: Gen[ExactYear] = Gen.posNum[Int].map(ExactYear)

  def exactMonthGen: Gen[ExactMonth] = Gen.posNum[Int].map(ExactMonth)

  def exactDayGen: Gen[ExactDay] = Gen.posNum[Int].map(ExactDay)

  def concreteDateGen: Gen[ConcreteDate] =
    for {
      year  <- Gen.oneOf(exactYearGen, Gen.const(AnyYear), Gen.const(Next), Gen.const(Previous))
      month <- Gen.oneOf(exactMonthGen, Gen.const(AnyMonth))
      day   <- Gen.oneOf(exactDayGen, Gen.const(AnyDay), Gen.const(FirstDay), Gen.const(LastDay))
    } yield ConcreteDate(year, month, day)

  def dateFieldGen: Gen[DateField] = FormComponentGen.formComponentIdGen.map(DateField)

  def offsetDateGen: Gen[OffsetDate] = Gen.posNum[Int].map(OffsetDate(_))

  def dateConstraintInfoGen: Gen[DateConstraintInfo] = Gen.oneOf(
    Gen.const(Today),
    concreteDateGen,
    dateFieldGen
  )

  def dateConstraintGen: Gen[DateConstraint] =
    for {
      beforeOrAfter <- beforeOrAfterOrPreciselyGen
      format        <- dateConstraintInfoGen
      offset        <- offsetDateGen
    } yield DateConstraint(beforeOrAfter, format, offset)

  def dateConstraintTypeGen: Gen[DateConstraintType] = Gen.oneOf(
    Gen.const(AnyDate),
    Gen.listOfN(2, dateConstraintGen).map(DateConstraints)
  )
}

object FormatExprGen extends FormatExprGen
