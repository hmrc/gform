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

import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.formtemplate.EmailVerification
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }

trait FormatParser extends ValueParser {

  lazy val fortmatExpr: RoundingMode => Option[List[SelectionCriteria]] => EmailVerification => Parser[FormatExpr] =
    rm =>
      selectionCriteria =>
        emailVerification => {
          dateFormat |
            textFormat(rm)(selectionCriteria)(emailVerification) |
            anyWordExpression
        }

  lazy val dateFormat: Parser[DateFormat] = {
    anyDateConstraint ^^ { constraints =>
      DateFormat(constraints)
    } | dateConstraints ^^ { constraints =>
      DateFormat(constraints)
    }
  }

  lazy val dateConstraints: Parser[DateConstraints] = {
    dateConstraint ~ "," ~ dateConstraints ^^ { case x ~ _ ~ xs =>
      DateConstraints(x :: xs.constraints)
    } | dateConstraint ^^ { x =>
      DateConstraints(List(x))
    }
  }

  lazy val dateConstraint: Parser[DateConstraint] = {
    beforeAfterPreciselyParser ~ exactDateExpr ~ offsetExpression ^^ { case beforeOrAfter ~ dateExpr ~ offset =>
      DateConstraint(beforeOrAfter, dateExpr, offset)
    } | beforeAfterPreciselyParser ~ exactDateExpr ^^ { case beforeOrAfter ~ dateExpr =>
      DateConstraint(beforeOrAfter, dateExpr, OffsetDate(0))
    }
  }

  lazy val anyDateConstraint: Parser[DateConstraintType] = "anyDate" ^^^ AnyDate

  lazy val beforeAfterPreciselyParser: Parser[BeforeAfterPrecisely] = (
    "after" ^^^ After
      | "before" ^^^ Before
      | "precisely" ^^^ Precisely
  )

  lazy val exactDateExpr: Parser[DateConstraintInfo] = (
    "today" ^^^ Today
      | yearParser ~ delimiter ~ monthParser ~ delimiter ~ dayParser ^^ { case year ~ _ ~ month ~ _ ~ day =>
        ConcreteDate(year, month, day)
      } | "${" ~> formatParserAlphabeticOnly <~ "}" ^^ { field =>
        DateField(FormComponentId(field))
      }
  )

  lazy val anyWordExpression: Parser[FormatExpr] = anyWordFormat ^^ { anyWord =>
    OrientationFormat(anyWord)
  }

  lazy val offsetExpression: Parser[OffsetDate] = anyInteger ^^ { offset =>
    OffsetDate(offset)
  }

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

  lazy val countryCodeFormat: Parser[TextFormat] = (
    "countryCode" ^^^ TextFormat(CountryCode)
      | "nonUkCountryCode" ^^^ TextFormat(NonUkCountryCode)
  )

  lazy val basicFormat: Option[List[SelectionCriteria]] => Parser[TextFormat] =
    selectionCriteria => {
      "shortText(" ~> positiveInteger ~ "," ~ positiveInteger <~ ")" ^^ { case min ~ _ ~ max =>
        TextFormat(ShortText(min, max))
      } | "shortText" ^^^ TextFormat(ShortText.default) |
        "text(" ~> positiveInteger ~ "," ~ positiveInteger <~ ")" ^^ { case min ~ _ ~ max =>
          TextFormat(TextWithRestrictions(min, max))
        } | "text" ^^^ TextFormat(TextConstraint.default) |
        "lookup(" ~> register ~ ")" ^^ { case register ~ _ =>
          TextFormat(Lookup(register, selectionCriteria))
        } | "submissionRef" ^^^ TextFormat(SubmissionRefFormat) |
        "referenceNumber(" ~> positiveInteger <~ ")" ^^ { min =>
          TextFormat(ReferenceNumber(min, min))
        } | "referenceNumber(" ~> positiveInteger ~ "," ~ positiveInteger <~ ")" ^^ { case min ~ _ ~ max =>
          TextFormat(ReferenceNumber(min, max))
        }
    }

  lazy val register: Parser[Register] = {
    "cashType" ^^^ Register.CashType |
      "country" ^^^ Register.Country |
      "currency" ^^^ Register.Currency |
      "intercept" ^^^ Register.Intercept |
      "port" ^^^ Register.Port |
      "transportMode" ^^^ Register.TransportMode |
      "originWho" ^^^ Register.OriginWho |
      "originMainPart" ^^^ Register.OriginMainPart |
      "originSellingSomething" ^^^ Register.OriginSellingSomething |
      "originSavingsEarnings" ^^^ Register.OriginSavingsEarnings |
      "origin" ^^^ Register.Origin |
      "intentBuyingWhat" ^^^ Register.IntentBuyingWhat |
      "intentBusiness" ^^^ Register.IntentBusiness |
      "intentLivingCostsAndFees" ^^^ Register.IntentLivingCostsAndFees |
      "intentOther" ^^^ Register.IntentOther |
      "intentBigPurchase" ^^^ Register.IntentBigPurchase |
      "intent" ^^^ Register.Intent |
      "sicCode" ^^^ Register.SicCode
  }

  lazy val contactFormat: EmailVerification => Parser[TextFormat] = emailVerification => {
    "telephoneNumber" ^^^ TextFormat(TelephoneNumber) | "email" ^^^ TextFormat(emailVerification.textConstraint)
  }

  lazy val numberFormat: RoundingMode => Parser[TextFormat] = rm => {
    "number" ~> numberArgs ^^ { na =>
      TextFormat(Number(maxWholeDigits = na._1, maxFractionalDigits = na._2, rm, unit = na._3))
    } | "number" ^^^ TextFormat(Number(TextConstraint.defaultWholeDigits, TextConstraint.defaultFractionalDigits, rm))
  }

  lazy val positiveNumberFormat: RoundingMode => Parser[TextFormat] = rm => {
    "positiveNumber" ~> numberArgs ^^ { na =>
      TextFormat(PositiveNumber(maxWholeDigits = na._1, maxFractionalDigits = na._2, rm, unit = na._3))
    } | "positiveNumber" ^^^ TextFormat(
      PositiveNumber(TextConstraint.defaultWholeDigits, TextConstraint.defaultFractionalDigits, rm)
    )
  }

  lazy val governmentIdFormat: Parser[TextFormat] = {
    "utr" ^^^ TextFormat(UTR) |
      "nino" ^^^ TextFormat(NINO) |
      "ukVrn" ^^^ TextFormat(UkVrn) |
      "companyRegistrationNumber" ^^^ TextFormat(CompanyRegistrationNumber) |
      "EORI" ^^^ TextFormat(EORI) |
      "UkEORI" ^^^ TextFormat(UkEORI) |
      "childBenefitNumber" ^^^ TextFormat(ChildBenefitNumber) |
      "payeReference" ^^^ TextFormat(PayeReference)
  }

  lazy val positiveWholeNumberFormat: RoundingMode => Parser[TextFormat] = rm =>
    "positiveWholeNumber" ^^^ TextFormat(PositiveNumber(maxFractionalDigits = 0, roundingMode = rm))

  lazy val moneyFormat: RoundingMode => Parser[TextFormat] = rm => {
    "sterling" ^^^ TextFormat(Sterling(rm, false)) |
      "positiveSterling" ^^^ TextFormat(Sterling(rm, true)) |
      "wholePositiveSterling" ^^^ TextFormat(WholeSterling(true)) |
      "ukBankAccountNumber" ^^^ TextFormat(UkBankAccountNumber) |
      "ukSortCode" ^^^ TextFormat(UkSortCodeFormat)
  }

  lazy val numberArgs: Parser[(Int, Int, Option[LocalisedString])] = {
    wholeFractional ~ "," ~ localisedString ~ ")" ^^ { case wf ~ _ ~ q ~ _ =>
      (wf.w, wf.f, Option(q))
    } | wholeFractional <~ ")" ^^ { wf =>
      (wf.w, wf.f, None)
    }
  }

  lazy val wholeFractional: Parser[WholeFractional] = "(" ~> positiveInteger ~ "," ~ positiveInteger ^^ {
    case whole ~ _ ~ fractional =>
      WholeFractional(whole, fractional)
  }

  case class WholeFractional(w: Int, f: Int)

  lazy val quotedString: Parser[String] = "'" ~> "[^']+".r <~ "'"

  lazy val localisedString: Parser[LocalisedString] = english ~ "," ~ welsh ^^ { case en ~ _ ~ cy =>
    LocalisedString(en ++ cy)
  } | quotedString.map(value => LocalisedString(Map(LangADT.En -> value)))

  def english: Parser[Map[LangADT, String]] = "'" ~> "en" ~ "'" ~ ":" ~ quotedString ^^ { case _ ~ _ ~ _ ~ en =>
    Map(LangADT.En -> en)
  }

  def welsh: Parser[Map[LangADT, String]] = "'" ~> "cy" ~ "'" ~ ":" ~ quotedString ^^ { case _ ~ _ ~ _ ~ cy =>
    Map(LangADT.Cy -> cy)
  }
  //"format": "positiveNumber(11, 2, 'en':'litres','cy':'litrau')"
}

case object FormatParser extends FormatParser with ParsingHelper {

  def validate(
    rm: RoundingMode,
    selectionCriteria: Option[List[SelectionCriteria]],
    emailVerification: EmailVerification
  )(expression: String): Opt[FormatExpr] =
    validateWithParser(expression, fortmatExpr(rm)(selectionCriteria)(emailVerification))

}
