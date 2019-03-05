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

import java.time.LocalDate

import parseback._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.BasicParsers._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ ChoiceExpression, DateExpression, TextExpression, ValueExpr, _ }
import pureconfig.generic.auto._ // It is now necessary to import `pureconfig.generic.auto._` everywhere a config is loaded or written, even though IntelliJ sees this as unused, its still required

object ValueParser {

  implicit val W = Whitespace(() | """\s+""".r)

  case class IsSubtraction(value: Boolean)
  val isSubtraction =
    pureconfig.loadConfigOrThrow[IsSubtraction]("feature.subtraction").value
  def validate(expression: String): Opt[ValueExpr] =
    validateWithParser(expression, exprDeterminer)

  lazy val exprDeterminer: Parser[ValueExpr] = (dateExpression ^^ ((loc, expr) => DateExpression(expr))
    | positiveIntegers ^^ ((loc, selections) => ChoiceExpression(selections))
    | expr ^^ ((loc, expr) => TextExpression(expr)))

  lazy val dateExpression: Parser[DateValue] = (nextDate | lastDate | exactDate | today) ^^ { (loc, dateExpr) =>
    dateExpr
  }

  lazy val today: Parser[TodayDateValue.type] = "today" ^^ const(TodayDateValue)

  lazy val exactDate: Parser[ExactDateValue] = exactYearParser ~ exactMonthDay ^^ { (loc, year, month, day) =>
    ExactDateValue(year, month, day)
  } | exactYearMonth ~ "firstDay" ^^ { (loc, year, month, _) =>
    ExactDateValue(year, month, 1)
  } | exactYearMonth ~ "lastDay" ^^ { (loc, year, month, _) =>
    ExactDateValue(year, month, LocalDate.of(year, month, 1).lengthOfMonth)
  }

  lazy val nextDate: Parser[NextDateValue] =
    nextOrPreviousValue("next", NextDateValue.apply)

  lazy val lastDate: Parser[PreviousDateValue] =
    nextOrPreviousValue("last", PreviousDateValue.apply)

  lazy val exprFormCtx: Parser[Expr] = (quotedConstant
    | parserExpression)

  lazy val expr: Parser[Expr] = (quotedConstant
    | "${" ~> parserExpression <~ "}")

  lazy val operation: Parser[Operation] = (
    "+" ^^ const(Addition)
      | "*" ^^ const(Multiplication)
  )

  lazy val contextField: Parser[Expr] = ("eeitt" ~ "." ~ eeitt ^^ { (loc, _, _, eeitt) =>
    EeittCtx(eeitt)
  }
    | "user" ~ "." ~ userField ^^ { (loc, _, _, userField) =>
      UserCtx(userField)
    }
    | "form" ~ "." ~ "submissionReference" ^^ { (loc, _, _, fieldName) =>
      SubmissionReference
    }
    | "form" ~ "." ~ alphabeticOnly ^^ { (loc, _, _, fieldName) =>
      FormCtx(fieldName)
    }
    | "auth" ~ "." ~ authInfo ^^ { (loc, _, _, authInfo) =>
      AuthCtx(authInfo)
    }
    | "hmrcRosmRegistrationCheck" ~ "." ~ rosmProp ^^ { (loc, _, _, rosmProp) =>
      HmrcRosmRegistrationCheck(rosmProp)
    }
    | quotedConstant

    | alphabeticOnly ~ ".sum" ^^ { (loc, value, _) =>
      Sum(FormCtx(value))
    }
    | anyDigitConst ^^ { (loc, str) =>
      str
    }
    | alphabeticOnly ^^ { (loc, fn) =>
      FormCtx(fn)
    })

  lazy val parserExpression: Parser[Expr] = (parserExpression ~ "+" ~ parserExpression ^^ { (loc, expr1, _, expr2) =>
    Add(expr1, expr2)
  }
    | subtractionFeatureSwitch)

  lazy val subtractionFeatureSwitch: Parser[Expr] =
    if (isSubtraction) {
      (parserExpression ~ "-" ~ parserExpression ^^ { (loc, expr1, _, expr2) =>
        Subtraction(expr1, expr2)
      }
        | product)
    } else {
      product
    }

  lazy val product: Parser[Expr] = (product ~ "*" ~ product ^^ { (loc, expr1, _, expr2) =>
    Multiply(expr1, expr2)
  }
    | contextField)

  lazy val alphabeticOnly: Parser[String] = """[a-zA-Z]\w*""".r ^^ { (loc, str) =>
    str
  }
  lazy val quotedConstant: Parser[Expr] = ("'" ~ anyConstant ~ "'" ^^ { (loc, _, str, _) =>
    str
  }
    |
      "''".r ^^ { (loc, value) =>
        Constant("")
      })

  lazy val anyConstant: Parser[Constant] = ("""[ \w,]+""".r ^^ { (loc, str) =>
    Constant(str)
  })

  lazy val anyDigitConst: Parser[Expr] = (
    // parse single digit, e.g. "9"
    """\d""".r ^^ { (loc, str) =>
      Constant(str)
    }
    // parse two or more digits with commas as thousands separators digit, e.g. "9,876"
      | """\d[\d,]*[\d]""".r ^^ { (loc, str) =>
        Constant(str)
      }
    // parse decimal fraction, e.g. ".56"
      | """\.[\d]*\d""".r ^^ { (loc, str) =>
        Constant(str)
      }
    // parse number plus decimal fraction, e.g. "1,234.56"
      | """\d[\d,]*\.([\d]*\d)?""".r ^^ { (loc, str) =>
        Constant(str)
      }
  )

  lazy val eeitt: Parser[Eeitt] = (
    "businessUser" ^^ const(BusinessUser)
      | "agent" ^^ const(Agent)
      | "userId" ^^ const(UserId)
  )

  lazy val userField: Parser[UserField] = (
    "affinityGroup" ^^ const(AffinityGroup)
      | "enrolments" ~ "." ~ enrolment ^^ ((_, _, _, en) => en)
      | "enrolledIdentifier" ^^ const(EnrolledIdentifier)
  )

  lazy val enrolment: Parser[Enrolment] = serviceName ~ "." ~ identifierName ^^ { (_, sn, _, in) =>
    Enrolment(sn, in)
  }

  lazy val serviceName: Parser[ServiceName] = """[^.]+""".r ^^ { (loc, str) =>
    ServiceName(str)
  }

  lazy val identifierName: Parser[IdentifierName] = """[^.}]+""".r ^^ { (loc, str) =>
    IdentifierName(str)
  }

  lazy val authInfo: Parser[AuthInfo] = (
    "gg" ^^ const(GG)
      | "payenino" ^^ const(PayeNino)
      | "sautr" ^^ const(SaUtr)
      | "ctutr" ^^ const(CtUtr)
  )
  lazy val rosmProp: Parser[RosmProp] = (
    "safeId" ^^ const(RosmSafeId)
      | "organisationName" ^^ const(RosmOrganisationName)
      | "organisationType" ^^ const(RosmOrganisationType)
      | "isAGroup" ^^ const(RosmIsAGroup)
  )

  private def const[A](a: A)(loc: List[Line], matched: String): A = a
}
