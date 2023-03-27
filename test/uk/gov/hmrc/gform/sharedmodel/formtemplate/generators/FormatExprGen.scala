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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ After, AnyDate, Before, BeforeAfterPrecisely, ChildBenefitNumber, CompanyRegistrationNumber, ConcreteDate, CountryCode, CtUTR, DateConstraint, DateConstraintInfo, DateConstraintType, DateConstraints, DateField, Day, EORI, Email, Month, NINO, NonUkCountryCode, Number, OffsetDate, PayeReference, PositiveNumber, Precisely, RoundingMode, SaUTR, ShortText, Sterling, TelephoneNumber, TextConstraint, TextWithRestrictions, Today, UkBankAccountNumber, UkEORI, UkSortCodeFormat, UkVrn, Year }

trait FormatExprGen {
  def numberGen: Gen[Number] =
    for {
      maxWholeDigits      <- Gen.posNum[Int]
      maxFractionalDigits <- Gen.posNum[Int]
      units               <- Gen.option(LocalisedStringGen.localisedStringGen)
      roundingMode        <- roundingModeGen
    } yield Number(maxWholeDigits, maxFractionalDigits, roundingMode, units)

  def positiveNumberGen: Gen[PositiveNumber] =
    for {
      maxWholeDigits      <- Gen.posNum[Int]
      maxFractionalDigits <- Gen.posNum[Int]
      units               <- Gen.option(LocalisedStringGen.localisedStringGen)
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

  def shortText: Gen[ShortText] =
    for {
      min <- Gen.posNum[Int]
      max <- Gen.posNum[Int]
    } yield ShortText(min, max)

  def sterlingGen: Gen[Sterling] =
    PrimitiveGen.booleanGen.map(b => Sterling(RoundingMode.defaultRoundingMode, b))

  def textConstraintGen: Gen[TextConstraint] = Gen.oneOf(
    numberGen,
    positiveNumberGen,
    shortText,
    textWithRestrictions,
    sterlingGen,
    Gen.const(UkBankAccountNumber),
    Gen.const(UkSortCodeFormat),
    Gen.const(SaUTR),
    Gen.const(CtUTR),
    Gen.const(NINO),
    Gen.const(PayeReference),
    Gen.const(TelephoneNumber),
    Gen.const(Email),
    Gen.const(UkVrn),
    Gen.const(CountryCode),
    Gen.const(NonUkCountryCode),
    Gen.const(CompanyRegistrationNumber),
    Gen.const(EORI),
    Gen.const(UkEORI),
    Gen.const(ChildBenefitNumber)
  )

  def beforeOrAfterOrPreciselyGen: Gen[BeforeAfterPrecisely] = Gen.oneOf(Before, After, Precisely)

  def exactYearGen: Gen[Year.Exact] = Gen.posNum[Int].map(Year.Exact)

  def exactMonthGen: Gen[Month.Exact] = Gen.posNum[Int].map(Month.Exact)

  def exactDayGen: Gen[Day.Exact] = Gen.posNum[Int].map(Day.Exact)

  def concreteDateGen: Gen[ConcreteDate] =
    for {
      year  <- Gen.oneOf(exactYearGen, Gen.const(Year.Any), Gen.const(Year.Next), Gen.const(Year.Previous))
      month <- Gen.oneOf(exactMonthGen, Gen.const(Month.Any))
      day   <- Gen.oneOf(exactDayGen, Gen.const(Day.Any), Gen.const(Day.First), Gen.const(Day.Last))
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
