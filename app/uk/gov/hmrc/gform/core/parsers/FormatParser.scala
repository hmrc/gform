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

import parseback._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.BasicParsers._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object FormatParser {

  def validate(rm: RoundingMode)(expression: String): Opt[FormatExpr] = validateWithParser(expression, expr(rm))

  lazy val expr: RoundingMode => Parser[FormatExpr] = rm => {
    dateFormat |
      textFormat(rm) |
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
    beforeOrAfter ~ exactDateExpr ~ offsetExpression ^^ { (loc, beforeOrAfter, dateExpr, offset) =>
      DateConstraint(beforeOrAfter, dateExpr, offset)
    } | beforeOrAfter ~ exactDateExpr ^^ { (loc, beforeOrAfter, dateExpr) =>
      DateConstraint(beforeOrAfter, dateExpr, OffsetDate(0))
    } | "precisely" ~ abstractDateExpr ^^ { (loc, _, dateExpr) =>
      DateConstraint(Precisely, dateExpr, OffsetDate(0))
    }
  }

  lazy val anyDateConstraint: Parser[DateConstraintType] = "anyDate" ^^ { (loc, _) =>
    AnyDate
  }

  lazy val beforeOrAfter: Parser[BeforeAfterPrecisely] = {
    "after" ^^ { (loc, after) =>
      After
    } | "before" ^^ { (loc, before) =>
      Before
    }

  }

  lazy val exactDateExpr: Parser[DateConstraintInfo] = {
    "today" ^^ { (loc, today) =>
      Today
    } | exactYearParserWithNextAndPrevious ~ delimiter ~ exactMonthParser ~ delimiter ~ exactYearParserWithFirstAndLastDay ^^ {
      (_, year, _, month, _, day) =>
        ConcreteDate(year, ExactMonth(month), day)
    } | "${" ~ alphabeticOnly ~ "}" ^^ { (_, _, field, _) =>
      DateField(FormComponentId(field))
    }
  }

  lazy val abstractDateExpr: Parser[DateConstraintInfo] = {
    yearParser ~ monthDay ^^ { (loc, year, month, day) =>
      ConcreteDate(year, month, day)
    }
  }

  lazy val contextField: Parser[String] = alphabeticOnly ^^ { (loc, fn) =>
    fn
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

  lazy val textFormat: RoundingMode => Parser[FormatExpr] = rm => {
    numberFormat(rm) |
      positiveNumberFormat(rm) |
      positiveWholeNumberFormat(rm) |
      moneyFormat(rm) |
      contactFormat |
      governmentIdFormat |
      basicFormat |
      countryCodeFormat
  }

  lazy val countryCodeFormat: Parser[TextFormat] = {
    "countryCode" ^^ { (loc, _) =>
      TextFormat(CountryCode)
    } | "nonUkCountryCode" ^^ { (loc, _) =>
      TextFormat(NonUkCountryCode)
    }
  }

  lazy val basicFormat: Parser[TextFormat] = {
    "shortText" ^^ { (loc, _) =>
      TextFormat(ShortText.default)
    } | "shortText(" ~ positiveInteger ~ "," ~ positiveInteger ~ ")" ^^ { (_, _, min, _, max, _) =>
      TextFormat(ShortText(min, max))
    } | "text" ^^ { (_, _) =>
      TextFormat(BasicText)
    } | "text(" ~ positiveInteger ~ "," ~ positiveInteger ~ ")" ^^ { (_, _, min, _, max, _) =>
      TextFormat(TextWithRestrictions(min, max))
    } | "lookup(" ~ register ~ ")" ^^ { (_, _, register, _) =>
      TextFormat(Lookup(register))
    } | "submissionRef" ^^ { (_, _) =>
      TextFormat(SubmissionRefFormat)
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

  lazy val contactFormat: Parser[TextFormat] = {
    "telephoneNumber" ^^ { (loc, _) =>
      TextFormat(TelephoneNumber)
    } | "email" ^^ { (loc, _) =>
      TextFormat(Email)
    }
  }

  lazy val numberFormat: RoundingMode => Parser[TextFormat] = rm => {
    "number" ~ numberArgs ^^ { (loc, _, na) =>
      TextFormat(Number(maxWholeDigits = na._1, maxFractionalDigits = na._2, rm, unit = na._3))
    } | "number" ^^ { (loc, _) =>
      TextFormat(
        Number(
          TextConstraint.defaultWholeDigits,
          TextConstraint.defaultFactionalDigits,
          RoundingMode.defaultRoundingMode))
    }
  }

  lazy val positiveNumberFormat: RoundingMode => Parser[TextFormat] = rm => {
    "positiveNumber" ~ numberArgs ^^ { (loc, _, na) =>
      TextFormat(PositiveNumber(maxWholeDigits = na._1, maxFractionalDigits = na._2, rm, unit = na._3))
    } | "positiveNumber" ^^ { (loc, _) =>
      TextFormat(
        PositiveNumber(
          TextConstraint.defaultWholeDigits,
          TextConstraint.defaultFactionalDigits,
          RoundingMode.defaultRoundingMode))
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
      TextFormat(EORI)
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
    } | "ukBankAccountNumber" ^^ { (loc, _) =>
      TextFormat(UkBankAccountNumber)
    } | "ukSortCode" ^^ { (loc, _) =>
      TextFormat(UkSortCodeFormat)
    }
  }

  lazy val numberArgs: Parser[(Int, Int, Option[String])] = {
    wholeFractional ~ "," ~ quotedString ~ ")" ^^ { (loc, wf, _, q, _) =>
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

}
