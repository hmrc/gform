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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ After, AnyDate, AnyText, AnyWord, BasicText, Before, BeforeOrAfter, ConcreteDate, CountryCode, DateConstraint, DateConstraintInfo, DateConstraintType, DateConstraints, DateField, Email, NINO, NextDate, NonUkCountryCode, Number, OffsetDate, PositiveNumber, PreviousDate, ShortText, Sterling, TelephoneNumber, TextConstraint, TextExpression, TextWithRestrictions, Today, UTR, UkBankAccountNumber, UkSortCodeFormat, UkVrn }

trait FormatExprGen {
  def numberGen: Gen[Number] =
    for {
      maxWholeDigits      <- Gen.posNum[Int]
      maxFractionalDigits <- Gen.posNum[Int]
      units               <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
    } yield Number(maxWholeDigits, maxFractionalDigits, units)

  def positiveNumberGen: Gen[PositiveNumber] =
    for {
      maxWholeDigits      <- Gen.posNum[Int]
      maxFractionalDigits <- Gen.posNum[Int]
      units               <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
    } yield PositiveNumber(maxWholeDigits, maxFractionalDigits, units)

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
    Gen.const(Sterling),
    Gen.const(UkBankAccountNumber),
    Gen.const(UkSortCodeFormat),
    Gen.const(UTR),
    Gen.const(NINO),
    Gen.const(TelephoneNumber),
    Gen.const(Email),
    Gen.const(UkVrn),
    Gen.const(CountryCode),
    Gen.const(NonUkCountryCode)
  )

  def textExpressionGen: Gen[TextExpression] = ExprGen.exprGen().map(TextExpression(_))

  def beforeOrAfterGen: Gen[BeforeOrAfter] = Gen.oneOf(Before, After)

  def concreteDateGen: Gen[ConcreteDate] =
    for {
      year  <- Gen.posNum[Int]
      month <- Gen.posNum[Int]
      day   <- Gen.posNum[Int]
    } yield ConcreteDate(year, month, day)

  def nextDateGen: Gen[NextDate] =
    for {
      month <- Gen.posNum[Int]
      day   <- Gen.posNum[Int]
    } yield NextDate(month, day)

  def previousDateGen: Gen[PreviousDate] =
    for {
      month <- Gen.posNum[Int]
      day   <- Gen.posNum[Int]
    } yield PreviousDate(month, day)

  def anyWordGen: Gen[AnyWord] = Gen.alphaNumStr.map(AnyWord)

  def dateFieldGen: Gen[DateField] = FormComponentGen.formComponentIdGen.map(DateField)

  def offsetDateGen: Gen[OffsetDate] = Gen.posNum[Int].map(OffsetDate(_))

  def dateConstraintInfoGen: Gen[DateConstraintInfo] = Gen.oneOf(
    Gen.const(Today),
    concreteDateGen,
    nextDateGen,
    previousDateGen,
    anyWordGen,
    dateFieldGen
  )

  def dateConstraintGen: Gen[DateConstraint] =
    for {
      beforeOrAfter <- beforeOrAfterGen
      format        <- dateConstraintInfoGen
      offset        <- offsetDateGen
    } yield DateConstraint(beforeOrAfter, format, offset)

  def dateConstraintTypeGen: Gen[DateConstraintType] = Gen.oneOf(
    Gen.const(AnyDate),
    Gen.listOfN(2, dateConstraintGen).map(DateConstraints)
  )
}

object FormatExprGen extends FormatExprGen
