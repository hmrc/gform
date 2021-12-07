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

  lazy val dateFormat: Parser[DateFormat] = {
    anyDateConstraint ^^ { (loc, constraints) =>
      DateFormat(constraints)
    } | dateConstraints ^^ { (loc, constraints) =>
      DateFormat(constraints)
    }
  }

  lazy val dateConstraints: Parser[DateConstraints] = {
    dateConstraint ~ "," ~ dateConstraints ^^ { (loc, x, _, xs) =>
      DateConstraints(x :: xs.constraints)
    } | dateConstraint ^^ { (loc, x) =>
      DateConstraints(List(x))
    }
  }

  lazy val dateConstraint: Parser[DateConstraint] = {
    beforeAfterPreciselyParser ~ exactDateExpr ~ offsetExpression ^^ { (loc, beforeOrAfter, dateExpr, offset) =>
      DateConstraint(beforeOrAfter, dateExpr, offset)
    } | beforeAfterPreciselyParser ~ exactDateExpr ^^ { (loc, beforeOrAfter, dateExpr) =>
      DateConstraint(beforeOrAfter, dateExpr, OffsetDate(0))
    }
  }

  lazy val anyDateConstraint: Parser[DateConstraintType] = "anyDate" ^^ { (loc, _) =>
    AnyDate
  }

  lazy val beforeAfterPreciselyParser: Parser[BeforeAfterPrecisely] = {
    "after" ^^ { (loc, _) =>
      After
    } | "before" ^^ { (loc, _) =>
      Before
    } | "precisely" ^^ { (loc, _) =>
      Precisely
    }
  }

  lazy val exactDateExpr: Parser[DateConstraintInfo] = {
    "today" ^^ { (loc, today) =>
      Today
    } | yearParser ~ delimiter ~ monthParser ~ delimiter ~ dayParser ^^ { (_, year, _, month, _, day) =>
      ConcreteDate(year, month, day)
    } | "${" ~ alphabeticOnly ~ "}" ^^ { (_, _, field, _) =>
      DateField(FormComponentId(field))
    }
  }

  lazy val alphabeticOnly: Parser[String] = """\w+""".r ^^ { (loc, str) =>
    str
  }

  lazy val anyWordExpression: Parser[FormatExpr] = anyWordFormat ^^ { (loc, anyWord) =>
    OrientationFormat(anyWord)
  }

  lazy val offsetExpression: Parser[OffsetDate] = anyInteger ^^ { (loc, offset) =>
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

  lazy val countryCodeFormat: Parser[TextFormat] = {
    "countryCode" ^^ { (loc, _) =>
      TextFormat(CountryCode)
    } | "nonUkCountryCode" ^^ { (loc, _) =>
      TextFormat(NonUkCountryCode)
    }
  }

  lazy val basicFormat: Option[List[SelectionCriteria]] => Parser[TextFormat] =
    selectionCriteria => {
      "shortText" ^^ { (_, _) =>
        TextFormat(ShortText.default)
      } | "shortText(" ~ positiveInteger ~ "," ~ positiveInteger ~ ")" ^^ { (_, _, min, _, max, _) =>
        TextFormat(ShortText(min, max))
      } | "text(" ~ positiveInteger ~ "," ~ positiveInteger ~ ")" ^^ { (_, _, min, _, max, _) =>
        TextFormat(TextWithRestrictions(min, max))
      } | "text" ^^ { (_, _) =>
        TextFormat(TextConstraint.default)
      } | "lookup(" ~ register ~ ")" ^^ { (_, _, register, _) =>
        TextFormat(Lookup(register, selectionCriteria))
      } | "submissionRef" ^^ { (_, _) =>
        TextFormat(SubmissionRefFormat)
      } | "referenceNumber(" ~ positiveInteger ~ ")" ^^ { (_, _, min, _) =>
        TextFormat(ReferenceNumber(min, min))
      } | "referenceNumber(" ~ positiveInteger ~ "," ~ positiveInteger ~ ")" ^^ { (_, _, min, _, max, _) =>
        TextFormat(ReferenceNumber(min, max))
      }
    }

  lazy val register: Parser[Register] = {
    "cashType" ^^ { (loc, _) =>
      Register.CashType
    } |
      "country" ^^ { (loc, _) =>
        Register.Country
      } |
      "currency" ^^ { (loc, _) =>
        Register.Currency
      } |
      "intent" ^^ { (loc, _) =>
        Register.Intent
      } |
      "intercept" ^^ { (loc, _) =>
        Register.Intercept
      } |
      "origin" ^^ { (loc, _) =>
        Register.Origin
      } |
      "port" ^^ { (loc, _) =>
        Register.Port
      } |
      "transportMode" ^^ { (loc, _) =>
        Register.TransportMode
      } |
      "originWho" ^^ { (loc, _) =>
        Register.OriginWho
      } |
      "originMainPart" ^^ { (loc, _) =>
        Register.OriginMainPart
      } |
      "originSellingSomething" ^^ { (loc, _) =>
        Register.OriginSellingSomething
      } |
      "originSavingsEarnings" ^^ { (loc, _) =>
        Register.OriginSavingsEarnings
      } |
      "intentBuyingWhat" ^^ { (loc, _) =>
        Register.IntentBuyingWhat
      } |
      "intentBusiness" ^^ { (loc, _) =>
        Register.IntentBusiness
      } |
      "intentLivingCostsAndFees" ^^ { (loc, _) =>
        Register.IntentLivingCostsAndFees
      } |
      "intentOther" ^^ { (loc, _) =>
        Register.IntentOther
      } |
      "intentBigPurchase" ^^ { (loc, _) =>
        Register.IntentBigPurchase
      }
  }

  lazy val contactFormat: EmailVerification => Parser[TextFormat] = emailVerification => {
    "telephoneNumber" ^^ { (loc, _) =>
      TextFormat(TelephoneNumber)
    } | "email" ^^ { (loc, _) =>
      TextFormat(emailVerification.textConstraint)
    }
  }

  lazy val numberFormat: RoundingMode => Parser[TextFormat] = rm => {
    "number" ~ numberArgs ^^ { (loc, _, na) =>
      TextFormat(Number(maxWholeDigits = na._1, maxFractionalDigits = na._2, rm, unit = na._3))
    } | "number" ^^ { (loc, _) =>
      TextFormat(Number(TextConstraint.defaultWholeDigits, TextConstraint.defaultFractionalDigits, rm))
    }
  }

  lazy val positiveNumberFormat: RoundingMode => Parser[TextFormat] = rm => {
    "positiveNumber" ~ numberArgs ^^ { (loc, _, na) =>
      TextFormat(PositiveNumber(maxWholeDigits = na._1, maxFractionalDigits = na._2, rm, unit = na._3))
    } | "positiveNumber" ^^ { (loc, _) =>
      TextFormat(PositiveNumber(TextConstraint.defaultWholeDigits, TextConstraint.defaultFractionalDigits, rm))
    }
  }

  lazy val governmentIdFormat: Parser[TextFormat] = {
    "utr" ^^ { (loc, _) =>
      TextFormat(UTR)
    } | "nino" ^^ { (loc, _) =>
      TextFormat(NINO)
    } | "ukVrn" ^^ { (loc, _) =>
      TextFormat(UkVrn)
    } | "companyRegistrationNumber" ^^ { (loc, _) =>
      TextFormat(CompanyRegistrationNumber)
    } | "EORI" ^^ { (loc, _) =>
      TextFormat(EORI)
    } | "UkEORI" ^^ { (loc, _) =>
      TextFormat(UkEORI)
    } | "childBenefitNumber" ^^ { (loc, _) =>
      TextFormat(ChildBenefitNumber)
    }
  }

  lazy val positiveWholeNumberFormat: RoundingMode => Parser[TextFormat] = rm =>
    "positiveWholeNumber" ^^ { (loc, _) =>
      TextFormat(PositiveNumber(maxFractionalDigits = 0, roundingMode = rm))
    }

  lazy val moneyFormat: RoundingMode => Parser[TextFormat] = rm => {
    "sterling" ^^ { (loc, _) =>
      TextFormat(Sterling(rm, false))
    } | "positiveSterling" ^^ { (loc, _) =>
      TextFormat(Sterling(rm, true))
    } | "wholePositiveSterling" ^^ { (loc, _) =>
      TextFormat(WholeSterling(true))
    } | "ukBankAccountNumber" ^^ { (loc, _) =>
      TextFormat(UkBankAccountNumber)
    } | "ukSortCode" ^^ { (loc, _) =>
      TextFormat(UkSortCodeFormat)
    }
  }

  lazy val numberArgs: Parser[(Int, Int, Option[LocalisedString])] = {
    wholeFractional ~ "," ~ localisedString ~ ")" ^^ { (loc, wf, _, q, _) =>
      (wf.w, wf.f, Option(q))
    } | wholeFractional ~ ")" ^^ { (loc, wf, _) =>
      (wf.w, wf.f, None)
    }
  }

  lazy val wholeFractional: Parser[WholeFractional] = "(" ~ positiveInteger ~ "," ~ positiveInteger ^^ {
    (loc, _, whole, _, fractional) =>
      WholeFractional(whole, fractional)
  }

  case class WholeFractional(w: Int, f: Int)

  lazy val quotedString: Parser[String] = "'" ~ "[^']+".r ~ "'" ^^ { (loc, _, s, _) =>
    s

  }
  lazy val localisedString: Parser[LocalisedString] = english ~ "," ~ welsh ^^ { (loc, en, _, cy) =>
    LocalisedString(en ++ cy)
  } | quotedString.map(value => LocalisedString(Map(LangADT.En -> value)))

  def english: Parser[Map[LangADT, String]] = "'" ~ "en" ~ "'" ~ ":" ~ quotedString ^^ { (loc, _, s, _, _, en) =>
    Map(LangADT.En -> en)
  }

  def welsh: Parser[Map[LangADT, String]] = "'" ~ "cy" ~ "'" ~ ":" ~ quotedString ^^ { (loc, _, s, _, _, cy) =>
    Map(LangADT.Cy -> cy)
  }
  //"format": "positiveNumber(11, 2, 'en':'litres','cy':'litrau')"
}
