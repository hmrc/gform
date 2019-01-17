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
import uk.gov.hmrc.gform.sharedmodel._
import BasicParsers._
import uk.gov.hmrc.gform.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object FormatParser {

  def validate(expression: String): Opt[FormatExpr] = validateWithParser(expression, expr)

  lazy val expr: Parser[FormatExpr] = {
    dateFormat |
      textFormat |
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
    beforeOrAfter ~ dateExpr ~ offsetExpression ^^ { (loc, beforeOrAfter, dateExpr, offset) =>
      DateConstraint(beforeOrAfter, dateExpr, offset)
    } | beforeOrAfter ~ dateExpr ^^ { (loc, beforeOrAfter, dateExpr) =>
      DateConstraint(beforeOrAfter, dateExpr, OffsetDate(0))
    }
  }

  lazy val anyDateConstraint: Parser[DateConstraintType] = "anyDate" ^^ { (loc, _) =>
    AnyDate
  }

  lazy val nextDate: Parser[NextDate] = nextOrPrevious("next", NextDate.apply)

  lazy val previousDate: Parser[PreviousDate] = nextOrPrevious("previous", PreviousDate.apply)

  lazy val beforeOrAfter: Parser[BeforeOrAfter] = {
    "after" ^^ { (loc, after) =>
      After
    } | "before" ^^ { (loc, before) =>
      Before
    }
  }

  lazy val dateExpr: Parser[DateConstraintInfo] = {
    "today" ^^ { (loc, today) =>
      Today
    } | yearParser ~ monthDay ^^ { (loc, year, month, day) =>
      ConcreteDate(year, month, day)
    } | "${" ~ alphabeticOnly ~ "}" ^^ { (loc, _, field, _) =>
      DateField(FormComponentId(field))
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

  lazy val textFormat: Parser[FormatExpr] = {
    numberFormat |
      positiveNumberFormat |
      positiveWholeNumberFormat |
      moneyFormat |
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
      TextFormat(ShortText)
    } | "text" ^^ { (loc, _) =>
      TextFormat(BasicText)
    } | "text(" ~ positiveInteger ~ "," ~ positiveInteger ~ ")" ^^ { (loc, _, min, _, max, _) =>
      TextFormat(TextWithRestrictions(min, max))
    }
  }

  lazy val contactFormat: Parser[TextFormat] = {
    "telephoneNumber" ^^ { (loc, _) =>
      TextFormat(TelephoneNumber)
    } | "email" ^^ { (loc, _) =>
      TextFormat(Email)
    }
  }

  lazy val numberFormat: Parser[TextFormat] = {
    "number" ~ numberArgs ^^ { (loc, _, na) =>
      TextFormat(Number(maxWholeDigits = na._1, maxFractionalDigits = na._2, roundingMode = na._3, unit = na._4))
    } | "number" ^^ { (loc, _) =>
      TextFormat(
        Number(
          TextConstraint.defaultWholeDigits,
          TextConstraint.defaultFactionalDigits,
          RoundingMode.defaultRoundingMode))
    }
  }

  lazy val positiveNumberFormat: Parser[TextFormat] = {
    "positiveNumber" ~ numberArgs ^^ { (loc, _, na) =>
      TextFormat(
        PositiveNumber(maxWholeDigits = na._1, maxFractionalDigits = na._2, roundingMode = na._3, unit = na._4))
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
    }
  }

  lazy val positiveWholeNumberFormat: Parser[TextFormat] = "positiveWholeNumber" ^^ { (loc, _) =>
    TextFormat(PositiveNumber(maxFractionalDigits = 0, roundingMode = RoundingMode.defaultRoundingMode))
  }

  lazy val moneyFormat: Parser[TextFormat] = {
    "sterling" ^^ { (loc, _) =>
      TextFormat(Sterling)
    } | "ukBankAccountNumber" ^^ { (loc, _) =>
      TextFormat(UkBankAccountNumber)
    } | "ukSortCode" ^^ { (loc, _) =>
      TextFormat(UkSortCodeFormat)
    }
  }

  lazy val numberArgs: Parser[(Int, Int, RoundingMode, Option[String])] = {
    wholeFractional ~ commaPrefixedOptionalRoundingMode ~ "," ~ quotedString ~ ")" ^^ { (loc, wf, rm, _, q, _) =>
      (wf.w, wf.f, rm, Option(q))
    } | wholeFractional ~ commaPrefixedOptionalRoundingMode ~ ")" ^^ { (loc, wf, rm, _) =>
      (wf.w, wf.f, rm, None)
    }
  }

  lazy val commaPrefixedOptionalRoundingMode: Parser[RoundingMode] = {
    "," ~ roundingMode ^^ { (_, _, rm) =>
      rm
    } | unit(()) ^^ { (_, _) =>
      RoundingMode.defaultRoundingMode
    }
  }

  lazy val roundingMode: Parser[RoundingMode] = {
    "Up" ^^ { (_, _) =>
      RoundingMode.Up
    } |
      "Down" ^^ { (_, _) =>
        RoundingMode.Down
      } |
      "Ceiling" ^^ { (_, _) =>
        RoundingMode.Ceiling
      } |
      "Floor" ^^ { (_, _) =>
        RoundingMode.Floor
      } |
      "HalfUp" ^^ { (_, _) =>
        RoundingMode.HalfUp
      } |
      "HalfDown" ^^ { (_, _) =>
        RoundingMode.HalfDown
      } |
      "HalfEven" ^^ { (_, _) =>
        RoundingMode.HalfEven
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
