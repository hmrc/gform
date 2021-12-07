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

import cats.parse.Rfc5234.{ alpha, digit, sp }
import cats.parse.Parser
import cats.parse.Parser.{ char, string }
import uk.gov.hmrc.gform.core.parsers.BooleanExprParser.token
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.BasicParsers._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.formtemplate.EmailVerification
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }

object FormatParser {

  def validate(
    rm: RoundingMode,
    selectionCriteria: Option[List[SelectionCriteria]],
    emailVerification: EmailVerification
  )(expression: String): Opt[FormatExpr] =
    validateWithParser(expression, expr(rm)(selectionCriteria)(emailVerification))

  lazy val expr: RoundingMode => Option[List[SelectionCriteria]] => EmailVerification => Parser[FormatExpr] = rm =>
    selectionCriteria =>
      emailVerification => {
        dateFormat |
          textFormat(rm)(selectionCriteria)(emailVerification) |
          anyWordExpression
      }

  lazy val dateFormat: Parser[DateFormat] = anyDateConstraint.map(constraints => DateFormat(constraints)) |
    dateConstraints.map(constraints => DateFormat(constraints))

  lazy val dateConstraints: Parser[DateConstraints] = {
    ((dateConstraint <* token(",")) ~ dateConstraints).map { case (x, xs) =>
      DateConstraints(x :: xs.constraints)
    } | dateConstraint.map(x => DateConstraints(List(x)))
  }

  lazy val dateConstraint: Parser[DateConstraint] = {
    (beforeAfterPreciselyParser ~ exactDateExpr ~ offsetExpression).map { case ((beforeOrAfter, dateExpr), offset) =>
      DateConstraint(beforeOrAfter, dateExpr, offset)
    } | (beforeAfterPreciselyParser ~ exactDateExpr).map { case (beforeOrAfter, dateExpr) =>
      DateConstraint(beforeOrAfter, dateExpr, OffsetDate(0))
    }
  }

  lazy val anyDateConstraint: Parser[DateConstraintType] = token("anyDate").map(_ => AnyDate)

  lazy val beforeAfterPreciselyParser: Parser[BeforeAfterPrecisely] = {
    token("after").map(_ => After) |
      token("before").map(_ => Before) |
      token("precisely").map(_ => Precisely)
  }

  lazy val exactDateExpr: Parser[DateConstraintInfo] = {
    token("today").map(_ => Today) |
      ((((yearParser <* delimiter) ~ monthParser) <* delimiter) ~ dayParser).map { case ((year, month), day) =>
        ConcreteDate(year, month, day)
      } | ((token("${") *> alphabeticOnly) <* token("}")).map(field => DateField(FormComponentId(field)))
  }

  lazy val alphabeticOnly: Parser[String] = (alpha | digit | char('_')).rep.map(x => x.toList.toString())

  lazy val anyWordExpression: Parser[FormatExpr] = anyWordFormatParser.map(anyWord => OrientationFormat(anyWord))

  lazy val offsetExpression: Parser[OffsetDate] = anyInteger.map(offset => OffsetDate(offset))

  lazy val textFormat: RoundingMode => Option[List[SelectionCriteria]] => EmailVerification => Parser[FormatExpr] =
    rm =>
      selectionCriteria =>
        emailVerification => {
          numberFormat(rm) |
            positiveNumberFormat(rm) |
            positiveWholeNumberFormat(rm) |
            moneyFormat(rm) |
            contactFormat(emailVerification) |
            governmentIdFormat |
            basicFormat(selectionCriteria) |
            countryCodeFormat
        }

  lazy val countryCodeFormat: Parser[TextFormat] = {
    token("countryCode").map(_ => TextFormat(CountryCode)) |
      token("nonUkCountryCode").map(_ => TextFormat(NonUkCountryCode))
  }

  lazy val basicFormat: Option[List[SelectionCriteria]] => Parser[TextFormat] =
    selectionCriteria => {
      token("shortText").map(_ => TextFormat(ShortText.default)) |
        (((token("shortText(") *> positiveInteger) <* token(",")) ~ positiveInteger <* token(")")).map {
          case (min, max) => TextFormat(ShortText(min, max))
        } |
        (((token("text(") *> positiveInteger) <* token(",")) ~ positiveInteger <* token(")")).map { case (min, max) =>
          TextFormat(TextWithRestrictions(min, max))
        } |
        token("text").map(_ => TextFormat(TextConstraint.default)) |
        ((token("lookup(") *> register) <* token(")")).map(register =>
          TextFormat(Lookup(register, selectionCriteria))
        ) |
        token("submissionRef").map(_ => TextFormat(SubmissionRefFormat)) |
        ((token("referenceNumber(") *> positiveInteger) <* token(")")).map(min =>
          TextFormat(ReferenceNumber(min, min))
        ) |
        (((token("referenceNumber(") *> positiveInteger) <* token(",")) ~ positiveInteger <* token(")")).map {
          case (min, max) => TextFormat(ReferenceNumber(min, max))
        }
    }

  lazy val register: Parser[Register] = {
    token("cashType").map(_ => Register.CashType) |
      token("country").map(_ => Register.Country) |
      token("currency").map(_ => Register.Currency) |
      token("intent").map(_ => Register.Intent) |
      token("intercept").map(_ => Register.Intercept) |
      token("origin").map(_ => Register.Origin) |
      token("port").map(_ => Register.Port) |
      token("transportMode").map(_ => Register.TransportMode) |
      token("originWho").map(_ => Register.OriginWho) |
      token("originMainPart").map(_ => Register.OriginMainPart) |
      token("originSellingSomething").map(_ => Register.OriginSellingSomething)
    token("originSavingsEarnings").map(_ => Register.OriginSavingsEarnings)
    token("intentBuyingWhat").map(_ => Register.IntentBuyingWhat) |
      token("intentBusiness").map(_ => Register.IntentBusiness) |
      token("intentLivingCostsAndFees").map(_ => Register.IntentLivingCostsAndFees) |
      token("intentOther").map(_ => Register.IntentOther) |
      token("intentBigPurchase").map(_ => Register.IntentBigPurchase)
  }

  lazy val contactFormat: EmailVerification => Parser[TextFormat] = emailVerification => {
    token("telephoneNumber").map(_ => TextFormat(TelephoneNumber)) |
      token("email").map(_ => TextFormat(emailVerification.textConstraint))
  }

  lazy val numberFormat: RoundingMode => Parser[TextFormat] = rm => {
    (token("number") *> numberArgs).map(na =>
      TextFormat(Number(maxWholeDigits = na._1, maxFractionalDigits = na._2, rm, unit = na._3))
    ) |
      token("number").map(_ =>
        TextFormat(Number(TextConstraint.defaultWholeDigits, TextConstraint.defaultFractionalDigits, rm))
      )
  }

  lazy val positiveNumberFormat: RoundingMode => Parser[TextFormat] = rm => {
    (token("positiveNumber") *> numberArgs).map(na =>
      TextFormat(PositiveNumber(maxWholeDigits = na._1, maxFractionalDigits = na._2, rm, unit = na._3))
    )
    token("positiveNumber").map(_ =>
      TextFormat(PositiveNumber(TextConstraint.defaultWholeDigits, TextConstraint.defaultFractionalDigits, rm))
    )
  }

  lazy val governmentIdFormat: Parser[TextFormat] = {
    token("utr").map(_ => TextFormat(UTR)) |
      token("nino").map(_ => TextFormat(NINO)) |
      token("ukVrn").map(_ => TextFormat(UkVrn)) |
      token("companyRegistrationNumber").map(_ => TextFormat(CompanyRegistrationNumber)) |
      token("EORI").map(_ => TextFormat(EORI)) |
      token("UkEORI").map(_ => TextFormat(UkEORI))
    token("childBenefitNumber").map(_ => TextFormat(ChildBenefitNumber))
  }

  lazy val positiveWholeNumberFormat: RoundingMode => Parser[TextFormat] = rm =>
    token("positiveWholeNumber").map(_ => TextFormat(PositiveNumber(maxFractionalDigits = 0, roundingMode = rm)))

  lazy val moneyFormat: RoundingMode => Parser[TextFormat] = rm => {
    token("sterling").map(_ => TextFormat(Sterling(rm, false))) |
      token("positiveSterling").map(_ => TextFormat(Sterling(rm, true))) |
      token("wholePositiveSterling").map(_ => TextFormat(WholeSterling(true))) |
      token("ukBankAccountNumber").map(_ => TextFormat(UkBankAccountNumber)) |
      token("ukSortCode").map(_ => TextFormat(UkSortCodeFormat))
  }

  lazy val numberArgs: Parser[(Int, Int, Option[LocalisedString])] = {
    ((wholeFractional <* token(",")) ~ localisedString <* token(")")).map { case (wf, q) =>
      (wf.w, wf.f, Option(q))
    } |
      (wholeFractional <* token(")")).map(wf => (wf.w, wf.f, None))
  }

  lazy val wholeFractional: Parser[WholeFractional] =
    (((token("(") *> positiveInteger) <* token(",")) ~ positiveInteger).map { case (whole, fractional) =>
      WholeFractional(whole, fractional)
    }

  case class WholeFractional(w: Int, f: Int)

  lazy val quotedString: Parser[String] = Parser.charsWhile(x => x != '\'').surroundedBy(char('\''))

  lazy val localisedString: Parser[LocalisedString] = ((english <* token(",")) ~ welsh).map { case (en, cy) =>
    LocalisedString(en ++ cy)
  } | quotedString.map(value => LocalisedString(Map(LangADT.En -> value)))

  def english: Parser[Map[LangADT, String]] = (token("'en':") *> quotedString).map(en => Map(LangADT.En -> en))

  def welsh: Parser[Map[LangADT, String]] = (token("'cy':") *> quotedString).map(cy => Map(LangADT.Cy -> cy))

  //"format": "positiveNumber(11, 2, 'en':'litres','cy':'litrau')"
}
