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

import java.time.LocalDate
import cats.parse.Parser
import cats.parse.Rfc5234.{alpha, digit, sp}
import cats.parse.Parser
import cats.parse.Parser.{char, map0, string}
import uk.gov.hmrc.gform.core.parsers.BooleanExprParser.token
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.BasicParsers._
import uk.gov.hmrc.gform.sharedmodel.dblookup.CollectionName
import uk.gov.hmrc.gform.sharedmodel.formtemplate.InternalLink.PageLink
import uk.gov.hmrc.gform.sharedmodel.formtemplate.UserField.Enrolment
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{DataRetrieveAttribute, DataRetrieveId}

object ValueParser {



  def validate(expression: String): Opt[ValueExpr] =
    validateWithParser(expression, exprDeterminer).right.map(_.rewrite)

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

  lazy val exprFormCtx: Parser[Expr] = (quotedLocalisedConstant
    | parserExpression)

  lazy val dateExprExactParser: Parser[DateExpr] = (exactDayParser ~ exactMonthParser ~ exactYearParser).map {
    case ((day, month), year) => DateValueExpr(ExactDateExprValue(year, month, day))
  }

  lazy val dateExprExactQuoted: Parser[DateExpr] = ((char('\'') *> dateExprExactParser) <* char('\'')) | dateExprExactParser

  lazy val signedInt: Parser[Int] = (plusOrMinus ~ positiveInteger).map{ case (plusOrMinus, i) =>
    i * (plusOrMinus match {
      case "+" => 1
      case "-" => -1
    })
  }

  lazy val signedYear: Parser[OffsetUnit] = (signedInt <* token("y")).map( year => OffsetUnit.Year(year))

  lazy val signedMonth: Parser[OffsetUnit] = (signedInt <* token("m")).map( month => OffsetUnit.Month(month))

  lazy val signedDay: Parser[OffsetUnit] = (signedInt <* token("d")).map(day => OffsetUnit.Day(day))

  private val offsets = List(signedYear, signedMonth, signedDay)

  private val perms1: List[Parser[OffsetYMD]] = offsets.map { ap =>
    ap.map(a => OffsetYMD(a))
  }

  private val perms2: Iterator[Parser[OffsetYMD]] =
    offsets.combinations(2).flatMap(_.permutations).map { case List(ap, bp) =>
      (ap ~ bp).map{ case (a, b) =>
        OffsetYMD(a, b)
      }
    }

  private val perms3: Iterator[Parser[OffsetYMD]] = offsets.permutations.map { case List(ap, bp, cp) =>
    (ap ~ bp ~ cp).map{case ((a, b), c) =>
      OffsetYMD(a, b, c)
    }
  }

  private val allYMDVariations = perms1 ++ perms2 ++ perms3

  lazy val offsetYMD: Parser[OffsetYMD] = allYMDVariations.reduce(_ | _)

  lazy val dateExprTODAY: Parser[DateExpr] = token("TODAY").map(_ => DateValueExpr(TodayDateExprValue))

  lazy val dateExprTODAYOffset: Parser[DateExpr] = (dateExprTODAY ~ offsetYMD).map{case (dateExprToday, offsetYMD) =>
    DateExprWithOffset(dateExprToday, offsetYMD)
  } | dateExprTODAY

  lazy val formCtxFieldDateWithOffset: Parser[DateExprWithOffset] = (formCtxFieldDate ~ offsetYMD).map {
    case (dateExprCtx, offsetYMD) => DateExprWithOffset(dateExprCtx, offsetYMD)
  }

  lazy val dateExpr: Parser[DateExpr] =
    dateExprExactQuoted | dateExprTODAYOffset | formCtxFieldDate | formCtxFieldDateWithOffset

  lazy val dateExprWithoutFormCtxFieldDate: Parser[DateExpr] =
    dateExprExactQuoted | dateExprTODAYOffset | formCtxFieldDateWithOffset

  lazy val dataSourceParse: Parser[DataSource] = (
    token("service.seiss").map( _ => DataSource.SeissEligible)
      | (token("mongo.") *> alphabeticOnly).map(name => DataSource.Mongo(CollectionName(name)))
      | (token("user.enrolments.") *> enrolment).map( enrolment => DataSource.Enrolment(enrolment.serviceName, enrolment.identifierName))
      | (token("delegated.classic.enrolments.") *> enrolment).map(enrolment =>
        DataSource.DelegatedEnrolment(enrolment.serviceName, enrolment.identifierName)
      )
  )

  lazy val expr: Parser[Expr] = (quotedConstant
    | (token("${") *> parserExpression) <* token("}"))

  lazy val internalLinkParser: Parser[InternalLink] = (
    token("printAcknowledgementPdf").map(_  =>  InternalLink.printAcknowledgementPdf)
    | token("printSummaryPdf").map( _ => InternalLink.printSummaryPdf)
    | (token("newForm.") *> FormTemplateId.unanchoredIdValidationParser).map(id => InternalLink.NewFormForTemplate(FormTemplateId(id)))
    | token("newForm").map( _ =>  InternalLink.newForm)
    | token("newSession").map(_  => InternalLink.newSession)
    | PageId.unanchoredIdValidationParser.map(id => PageLink(PageId(id)))
  )

  private lazy val periodFun = (((token("period(") *> dateExpr) <* token(",")) ~ dateExpr <* token(")"))

  private lazy val userFieldFunc: Parser[UserFieldFunc] = token("count").map(_ => UserFieldFunc.Count)|
    nonZeroPositiveInteger.map(i => UserFieldFunc.Index(i))

  private lazy val userEnrolmentFunc: Parser[UserCtx] =
    (((token("user.") *> userFieldEnrolments) <* token(".")) ~ userFieldFunc).map{ case (userField,func) =>
      UserCtx(Enrolment(userField.serviceName, userField.identifierName, Some(func)))
    }

  lazy val contextField: Parser[Expr] = userEnrolmentFunc | ((token("user.")  *> userField).map(userField => UserCtx(userField))
    | token("form.submissionReference").map(_ => FormTemplateCtx(FormTemplateProp.SubmissionReference))
    | token("form.id").map(_ => FormTemplateCtx(FormTemplateProp.Id))
    | token("form.lang").map(_ => LangCtx)
    | (token("form.") *> FormComponentId.unanchoredIdValidationParser).map( fieldName => FormCtx(FormComponentId(fieldName)))
    | (token("param.") *> alphabeticOnly).map(param => ParamCtx(QueryParam(param)))
    | (token("auth.") *> authInfo).map(authInfo => AuthCtx(authInfo))
    | (token("hmrcRosmRegistrationCheck.") *> rosmProp).map( rosmProp => HmrcRosmRegistrationCheck(rosmProp))
    | (token("link.") *> internalLinkParser).map(internalLink =>  LinkCtx(internalLink))
    | (((token("dataRetrieve.") *> DataRetrieveId.unanchoredIdValidationParser) <* token(".")) ~ DataRetrieveAttribute.unanchoredIdValidationparser).map {
      case (dataRetrieveId, dataRetrieveAttribute) =>
        DataRetrieveCtx(DataRetrieveId(dataRetrieveId), DataRetrieveAttribute.fromExpr(dataRetrieveAttribute))
    }
    | dateExprWithoutFormCtxFieldDate.map(
      DateCtx.apply
    ) // to parse date form fields with offset or date constants i.e TODAY, 01012020 etc (with or without offset)
    | periodValueParser ^^ { (loc, period) =>
      PeriodValue(period)
    }
    | (periodFun ~ "." ~ "sum|totalMonths|years|months|days".r mapWithLines {
      case (loc, _ ~ (dateExpr1: DateExpr) ~ _ ~ (dateExpr2: DateExpr) ~ _ ~ _ ~ prop) =>
        PeriodExt(
          Period(DateCtx(dateExpr1), DateCtx(dateExpr2)),
          prop match {
            case "sum"         => PeriodFn.Sum
            case "totalMonths" => PeriodFn.TotalMonths
            case "years"       => PeriodFn.Years
            case "months"      => PeriodFn.Months
            case "days"        => PeriodFn.Days
            case _ =>
              throw new IllegalArgumentException(
                "period(*,*).prop value is invalid. Allowed prop values are sum,totalMonths,years,months,days"
              )
          }
        )
    })
    | periodFun ^^ { (loc, _, dateExpr1, _, dateExpr2, _) =>
      Period(DateCtx(dateExpr1), DateCtx(dateExpr2))
    }
    | quotedLocalisedConstant
    | FormComponentId.unanchoredIdValidation ~ ".sum" ^^ { (loc, value, _) =>
      Sum(FormCtx(FormComponentId(value)))
    }
    | FormComponentId.unanchoredIdValidation ~ ".count" ^^ { (loc, value, _) =>
      Count(FormComponentId(value))
    }
    | FormComponentId.unanchoredIdValidation ~ "." ~ addressDetail ^^ { (loc, value, _, addressDetail) =>
      AddressLens(FormComponentId(value), addressDetail)
    }
    | anyDigitConst ^^ { (loc, str) =>
      str
    }
    | FormComponentId.unanchoredIdValidation ^^ { (loc, fn) =>
      FormCtx(FormComponentId(fn))
    })

  lazy val addressDetail: Parser[AddressDetail] =
    "line1" ^^^ AddressDetail.Line1 |
      "line2" ^^^ AddressDetail.Line2 |
      "line3" ^^^ AddressDetail.Line3 |
      "line4" ^^^ AddressDetail.Line4 |
      "postcode" ^^^ AddressDetail.Postcode |
      "country" ^^^ AddressDetail.Country

  lazy val formCtxFieldDate: Parser[DateExpr] = "form" ~ "." ~ FormComponentId.unanchoredIdValidation ^^ {
    (_, _, _, fieldName) =>
      DateFormCtxVar(FormCtx(FormComponentId(fieldName)))
  } | FormComponentId.unanchoredIdValidation ^^ { (_, fn) =>
    DateFormCtxVar(FormCtx(FormComponentId(fn)))
  }

  lazy val parserExpression: Parser[Expr] = "(" ~ addExpression ~ ")" ^^ { (loc, _, expr, _) =>
    expr
  } | addExpression

  lazy val addExpression: Parser[Expr] = (parserExpression ~ "+" ~ parserExpression ^^ { (loc, expr1, _, expr2) =>
    Add(expr1, expr2)
  }
    | subtractionExpression)

  lazy val subtractionExpression: Parser[Expr] =
    (parserExpression ~ "-" ~ parserExpression ^^ { (loc, expr1, _, expr2) =>
      Subtraction(expr1, expr2)
    }
      | product)

  lazy val product: Parser[Expr] = (parserExpression ~ "*" ~ parserExpression ^^ { (loc, expr1, _, expr2) =>
    Multiply(expr1, expr2)
  }
    | ifElseParser)

  lazy val ifElseParser: Parser[Expr] = {
    ("if" ~> BooleanExprParser.p4 ~ "then" ~ parserExpression ~ "else" ~ parserExpression ^^ {
      (_, cond, _, expr1, _, expr2) =>
        IfElse(cond, expr1, expr2)
    } | orElseParser)
  }

  lazy val orElseParser: Parser[Expr] = parserExpression ~ "else" ~ parserExpression ^^ { (loc, expr1, _, expr2) =>
    Else(expr1, expr2)
  } | contextField ^^ { (loc, value) =>
    value
  }

  lazy val alphabeticOnly: Parser[String] = """[a-zA-Z]\w*""".r ^^ { (loc, str) =>
    str
  }

  lazy val quotedLocalisedConstant: Parser[Expr] = (quotedConstant ~ "," ~ quotedConstant ^^ { (_, en, _, cy) =>
    IfElse(Equals(LangCtx, Constant("en")), en, cy)
  }
    |
    quotedConstant)

  lazy val quotedConstant: Parser[Expr] = ("'" ~ anyConstant ~ "'" ^^ { (loc, _, str, _) =>
    str
  }
    |
    "''".r ^^ { (loc, value) =>
      Constant("")
    })

  lazy val anyConstant: Parser[Constant] = """[^']+""".r ^^ { (loc, str) =>
    Constant(str)
  }

  lazy val anyDigitConst: Parser[Expr] = (
    // parse single digit, e.g. "9", "+9"
    """[+-]?\d""".r ^^ { (loc, str) =>
      Constant(str)
    }
    // parse two or more digits with commas as thousands separators digit, e.g. "9,876"
      | """[+-]?\d[\d,]*[\d]""".r ^^ { (loc, str) =>
        Constant(str)
      }
      // parse decimal fraction, e.g. ".56"
      | """[+-]?\.[\d]*\d""".r ^^ { (loc, str) =>
        Constant(str)
      }
      // parse number plus decimal fraction, e.g. "1,234.56"
      | """[+-]?\d[\d,]*\.([\d]*\d)?""".r ^^ { (loc, str) =>
        Constant(str)
      }
  )

  lazy val userFieldEnrolments: Parser[UserField.Enrolment] = "enrolments" ~ "." ~ enrolment ^^ ((_, _, _, en) => en)

  lazy val userField: Parser[UserField] = (
    "affinityGroup" ^^ const(UserField.AffinityGroup)
      | userFieldEnrolments
      | "enrolledIdentifier" ^^ const(UserField.EnrolledIdentifier)
  )

  lazy val enrolment: Parser[UserField.Enrolment] = serviceName ~ "." ~ identifierName ^^ { (_, sn, _, in) =>
    UserField.Enrolment(sn, in, None)
  }

  lazy val serviceName: Parser[ServiceName] = """[^.!= }]+""".r ^^ { (loc, str) =>
    ServiceName(str)
  }

  lazy val identifierName: Parser[IdentifierName] = """[^.!= }]+""".r ^^ { (loc, str) =>
    IdentifierName(str)
  }

  lazy val authInfo: Parser[AuthInfo] = (
    "gg" ^^ const(GG)
      | "payenino" ^^ const(PayeNino)
      | "sautr" ^^ const(SaUtr)
      | "ctutr" ^^ const(CtUtr)
      | "email" ^^ const(EmailId)
  )
  lazy val rosmProp: Parser[RosmProp] = (
    "safeId" ^^ const(RosmSafeId)
      | "organisationName" ^^ const(RosmOrganisationName)
      | "organisationType" ^^ const(RosmOrganisationType)
      | "isAGroup" ^^ const(RosmIsAGroup)
  )

  private def const[A](a: A)(loc: List[Line], matched: String): A = a
}
