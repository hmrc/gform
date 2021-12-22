/*
 * Copyright 2022 HM Revenue & Customs
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
import scala.util.parsing.combinator._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formtemplate.BooleanExprId
import uk.gov.hmrc.gform.sharedmodel.dblookup.CollectionName
import uk.gov.hmrc.gform.sharedmodel.formtemplate.InternalLink.PageLink
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SelectionCriteriaValue.{ SelectionCriteriaExpr, SelectionCriteriaReference, SelectionCriteriaSimpleValue }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.UserField.Enrolment
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieveAttribute, DataRetrieveId }

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.input.CharSequenceReader

trait ValueParser extends RegexParsers with PackratParsers with BasicParsers {

  //implicit val W = Whitespace(() | """\s+""".r)

  lazy val exprDeterminer: PackratParser[ValueExpr] = dateExpression ^^ (expr => DateExpression(expr)) |
    positiveIntegers ^^ (selections => ChoiceExpression(selections)) |
    expr ^^ (expr => TextExpression(expr))

  lazy val dateExpression: Parser[DateValue] = nextDate | lastDate | exactDate | today

  lazy val today: Parser[TodayDateValue.type] = "today" ^^^ TodayDateValue

  lazy val exactDate: Parser[ExactDateValue] = exactYearParser ~ exactMonthDay ^^ { case x =>
    ExactDateValue(x._1, x._2._1, x._2._2)
  } | exactYearMonth <~ "firstDay" ^^ { case (year, month) =>
    ExactDateValue(year, month, 1)
  } | exactYearMonth <~ "lastDay" ^^ { case (year, month) =>
    ExactDateValue(year, month, LocalDate.of(year, month, 1).lengthOfMonth)
  }

  lazy val nextDate: Parser[NextDateValue] =
    nextOrPreviousValue("next", NextDateValue.apply)

  lazy val lastDate: Parser[PreviousDateValue] =
    nextOrPreviousValue("last", PreviousDateValue.apply)

  lazy val exprFormCtx: Parser[Expr] = (quotedLocalisedConstant
    | _expr1)

  lazy val dateExprExactParser: Parser[DateExpr] = exactDayParser ~ exactMonthParser ~ exactYearParser ^^ {
    case day ~ month ~ year => DateValueExpr(ExactDateExprValue(year, month, day))
  }

  lazy val dateExprExactQuoted: Parser[DateExpr] = "'" ~> dateExprExactParser <~ "'" ^^ { dateExpr =>
    dateExpr
  } | dateExprExactParser

  lazy val signedInt: Parser[Int] = plusOrMinus ~ positiveInteger ^^ { case plusOrMinus ~ i =>
    i * (plusOrMinus match {
      case "+" => 1
      case "-" => -1
    })
  }

  lazy val signedYear: Parser[OffsetUnit] = signedInt <~ "y" ^^ { year =>
    OffsetUnit.Year(year)
  }

  lazy val signedMonth: Parser[OffsetUnit] = signedInt <~ "m" ^^ { month =>
    OffsetUnit.Month(month)
  }

  lazy val signedDay: Parser[OffsetUnit] = signedInt <~ "d" ^^ { day =>
    OffsetUnit.Day(day)
  }

  private val offsets = List(signedYear, signedMonth, signedDay)

  private val perms1: List[Parser[OffsetYMD]] = offsets.map { ap =>
    ap ^^ { a => OffsetYMD(a) }
  }

  private val perms2: Iterator[Parser[OffsetYMD]] =
    offsets.combinations(2).flatMap(_.permutations).map { case List(ap, bp) =>
      ap ~ bp ^^ { case a ~ b =>
        OffsetYMD(a, b)
      }
    }

  private val perms3: Iterator[Parser[OffsetYMD]] = offsets.permutations.map { case List(ap, bp, cp) =>
    ap ~ bp ~ cp ^^ { case a ~ b ~ c =>
      OffsetYMD(a, b, c)
    }
  }

  private val allYMDVariations = perms3 ++ perms2 ++ perms1

  lazy val offsetYMD: Parser[OffsetYMD] = allYMDVariations.reduce(_ | _)

  lazy val dateExprTODAY: Parser[DateExpr] = "TODAY" ^^^ DateValueExpr(TodayDateExprValue)

  lazy val dateExprTODAYOffset: Parser[DateExpr] = dateExprTODAY ~ offsetYMD ^^ { case dateExprToday ~ offsetYMD =>
    DateExprWithOffset(dateExprToday, offsetYMD)
  } | dateExprTODAY

  lazy val formCtxFieldDateWithOffset: Parser[DateExprWithOffset] = formCtxFieldDate ~ offsetYMD ^^ {
    case dateExprCtx ~ offsetYMD =>
      DateExprWithOffset(dateExprCtx, offsetYMD)
  }

  lazy val dateExpr: Parser[DateExpr] =
    dateExprTODAYOffset | formCtxFieldDateWithOffset | dateExprExactQuoted | formCtxFieldDate

  lazy val dateExprWithoutFormCtxFieldDate: Parser[DateExpr] =
    dateExprExactQuoted | dateExprTODAYOffset | formCtxFieldDateWithOffset

  lazy val dataSourceParse: Parser[DataSource] = (
    "service" ~ "." ~ "seiss" ^^ { _ =>
      DataSource.SeissEligible
    }
      | "mongo" ~ "." ~ alphabeticOnly ^^ { case _ ~ _ ~ name =>
        DataSource.Mongo(CollectionName(name))
      }
      | "user" ~ "." ~ "enrolments" ~ "." ~ enrolment ^^ { case _ ~ _ ~ _ ~ enrolment =>
        DataSource.Enrolment(enrolment.serviceName, enrolment.identifierName)
      }
      | "delegated.classic.enrolments." ~> enrolment ^^ { enrolment =>
        DataSource.DelegatedEnrolment(enrolment.serviceName, enrolment.identifierName)
      }
  )

  lazy val expr: PackratParser[Expr] = quotedConstant | "${" ~> _expr1 <~ "}"
  //lazy val expr: PackratParser[Expr] = "${" ~> parserExpression <~ "}"

  lazy val internalLinkParser: Parser[InternalLink] = "printAcknowledgementPdf" ^^ { _ =>
    InternalLink.printAcknowledgementPdf
  } | "printSummaryPdf" ^^ { _ =>
    InternalLink.printSummaryPdf
  } | "newForm" ~ "." ~ FormTemplateId.unanchoredIdValidation ^^ { case _ ~ _ ~ id =>
    InternalLink.NewFormForTemplate(FormTemplateId(id))
  } | "newForm" ^^ { _ =>
    InternalLink.newForm
  } | "newSession" ^^ { _ =>
    InternalLink.newSession
  } | PageId.unanchoredIdValidation ^^ { id =>
    PageLink(PageId(id))
  }

  private lazy val periodFun = "period(" ~ dateExpr ~ "," ~ dateExpr ~ ")"

  private lazy val userFieldFunc: Parser[UserFieldFunc] = "count" ^^ { _ =>
    UserFieldFunc.Count
  } | nonZeroPositiveInteger ^^ { i =>
    UserFieldFunc.Index(i)
  }

  private lazy val userEnrolmentFunc: Parser[UserCtx] =
    "user" ~ "." ~ userFieldEnrolments ~ "." ~ userFieldFunc ^^ { case _ ~ _ ~ userField ~ _ ~ func =>
      UserCtx(Enrolment(userField.serviceName, userField.identifierName, Some(func)))
    }

  lazy val contextField: Parser[Expr] = userEnrolmentFunc | ("user" ~ "." ~ userField ^^ { case _ ~ _ ~ userField =>
    UserCtx(userField)
  }
    | "form" ~ "." ~ "submissionReference" ^^ { case _ ~ _ ~ fieldName =>
      FormTemplateCtx(FormTemplateProp.SubmissionReference)
    }
    | "form" ~ "." ~ "id" ^^ { _ =>
      FormTemplateCtx(FormTemplateProp.Id)
    }
    | "form" ~ "." ~ "lang" ^^ { _ =>
      LangCtx
    }
    | "form" ~ "." ~ FormComponentId.unanchoredIdValidation ^^ { case _ ~ _ ~ fieldName =>
      FormCtx(FormComponentId(fieldName))
    }
    | "param" ~ "." ~ alphabeticOnly ^^ { case _ ~ _ ~ param =>
      ParamCtx(QueryParam(param))
    }
    | "auth" ~ "." ~ authInfo ^^ { case _ ~ _ ~ authInfo =>
      AuthCtx(authInfo)
    }
    | "hmrcRosmRegistrationCheck" ~ "." ~ rosmProp ^^ { case _ ~ _ ~ rosmProp =>
      HmrcRosmRegistrationCheck(rosmProp)
    }
    | "link" ~ "." ~ internalLinkParser ^^ { case _ ~ _ ~ internalLink =>
      LinkCtx(internalLink)
    }
    | "dataRetrieve" ~ "." ~ DataRetrieveId.unanchoredIdValidation ~ "." ~ DataRetrieveAttribute.unanchoredIdValidation ^^ {
      case _ ~ _ ~ dataRetrieveId ~ _ ~ dataRetrieveAttribute =>
        DataRetrieveCtx(DataRetrieveId(dataRetrieveId), DataRetrieveAttribute.fromName(dataRetrieveAttribute))
    }
    | dateExprWithoutFormCtxFieldDate.map(
      DateCtx.apply
    ) // to parse date form fields with offset or date constants i.e TODAY, 01012020 etc (with or without offset)
    | periodValueParser ^^ { period =>
      PeriodValue(period)
    }
    | (periodFun ~ "." ~ "sum|totalMonths|years|months|days".r ^^ {
      case _ ~ (dateExpr1: DateExpr) ~ _ ~ (dateExpr2: DateExpr) ~ _ ~ _ ~ prop =>
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
    | periodFun ^^ { case _ ~ dateExpr1 ~ _ ~ dateExpr2 ~ _ =>
      Period(DateCtx(dateExpr1), DateCtx(dateExpr2))
    }
    | quotedLocalisedConstant
    | FormComponentId.unanchoredIdValidation <~ ".sum" ^^ { case value =>
      Sum(FormCtx(FormComponentId(value)))
    }
    | FormComponentId.unanchoredIdValidation <~ ".count" ^^ { value =>
      Count(FormComponentId(value))
    }

    | FormComponentId.unanchoredIdValidation ~ "." ~ positiveInteger <~ ".size" ^^ { case value ~ _ ~ index =>
      Size(FormComponentId(value), index)
    }
    | FormComponentId.unanchoredIdValidation ~ "." ~ addressDetail ^^ { case value ~ _ ~ addressDetail =>
      AddressLens(FormComponentId(value), addressDetail)
    }
    | anyDigitConst |
    FormComponentId.unanchoredIdValidation ^^ { fn =>
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
    case _ ~ _ ~ fieldName =>
      DateFormCtxVar(FormCtx(FormComponentId(fieldName)))
  } | FormComponentId.unanchoredIdValidation ^^ { fn =>
    DateFormCtxVar(FormCtx(FormComponentId(fn)))
  }
  /*
  lazy val parserExpression: PackratParser[Expr] = "(" ~> product <~ ")" ^^ { expr =>
    expr
  } | product


  lazy val product: PackratParser[Expr] = (parserExpression ~ "*" ~ parserExpression ^^ { case expr1 ~ _ ~ expr2 =>
    Multiply(expr1, expr2)
  } | addExpression)

  lazy val addExpression: PackratParser[Expr] =
    (parserExpression ~ "+" ~ parserExpression ^^ { case expr1 ~ _ ~ expr2 =>
      Add(expr1, expr2)
    } | subtractionExpression)

  lazy val subtractionExpression: PackratParser[Expr] =
    (parserExpression ~ "-" ~ parserExpression ^^ { case expr1 ~ _ ~ expr2 =>
      Subtraction(expr1, expr2)
    } | contextField)
*/

  lazy val _expr1: PackratParser[Expr] = "if" ~> p4 ~ "then" ~ _expr1 ~ "orElse" ~ _expr1 ^^ {
    case cond ~ _ ~ expr1 ~ _ ~ expr2 => IfElse(cond, expr1, expr2)
  } | _add

  lazy val _add: PackratParser[Expr] = chainl1(_term, "+" ^^^ Add | "-" ^^^ Subtraction)

  val _term: PackratParser[Expr] = chainl1(_else, "*" ^^^ Multiply)

  val _else: PackratParser[Expr] = chainl1(_factor, "else" ^^^ Else)

  val _factor: PackratParser[Expr] = contextField | "(" ~> _expr1 <~ ")"

//  lazy val ifElseParser: PackratParser[Expr] = {
//    ("if" ~> p4 ~ "then" ~ parserExpression ~ "else" ~ parserExpression ^^ { case cond ~ _ ~ expr1 ~ _ ~ expr2 =>
//      IfElse(cond, expr1, expr2)
//    } | orElseParser)
//  }
//
//  lazy val orElseParser: PackratParser[Expr] = parserExpression ~ "else" ~ parserExpression ^^ {
//    case expr1 ~ _ ~ expr2 =>
//      Else(expr1, expr2)
//  } | contextField ^^ { value =>
//    value
//  }

  lazy val alphabeticOnly: Parser[String] = """[a-zA-Z]\w*""".r ^^ { str =>
    str
  }

  lazy val quotedLocalisedConstant: Parser[Expr] = (quotedConstant ~ "," ~ quotedConstant ^^ { case en ~ _ ~ cy =>
    IfElse(Equals(LangCtx, Constant("en")), en, cy)
  }
    |
    quotedConstant)

  lazy val quotedConstant: Parser[Expr] = "'" ~> anyConstant <~ "'" | "''".r ^^^ Constant("")

  lazy val anyConstant: Parser[Constant] = """[^']+""".r ^^ { str =>
    Constant(str)
  }

  lazy val anyDigitConst: Parser[Expr] = (
    """\d+\.""".r ^^ { str =>
      Constant(str)
    } | """[+-]?\d+""".r ^^ { str =>
      Constant(str)
    } |||
      // parse two or more digits with commas as thousands separators digit, e.g. "9,876"
      """[+-]?\d[\d,]*[\d]""".r ^^ { str =>
        Constant(str)
      }
      // parse decimal fraction, e.g. ".56"
      ||| """[+-]?\.[\d]*\d""".r ^^ { str =>
        Constant(str)
      }
      // parse number plus decimal fraction, e.g. "1,234.56"
      ||| """[+-]?\d[\d,]*\.([\d]*\d)?""".r ^^ { str =>
        Constant(str)
      }
  )

  lazy val userFieldEnrolments: Parser[UserField.Enrolment] = "enrolments" ~ "." ~ enrolment ^^ { case _ ~ _ ~ en =>
    en
  }

  lazy val userField: Parser[UserField] = (
    "affinityGroup" ^^ { _ => UserField.AffinityGroup }
      | userFieldEnrolments
      | "enrolledIdentifier" ^^ (_ => UserField.EnrolledIdentifier)
  )

  lazy val enrolment: Parser[UserField.Enrolment] = serviceName ~ "." ~ identifierName ^^ { case sn ~ _ ~ in =>
    UserField.Enrolment(sn, in, None)
  }

  lazy val serviceName: Parser[ServiceName] = """[^.!= }]+""".r ^^ { str =>
    ServiceName(str)
  }

  lazy val identifierName: Parser[IdentifierName] = """[^.!= }]+""".r ^^ { str =>
    IdentifierName(str)
  }

  lazy val authInfo: Parser[AuthInfo] = (
    "gg" ^^^ GG
      | "payenino" ^^^ PayeNino
      | "sautr" ^^^ SaUtr
      | "ctutr" ^^^ CtUtr
      | "email" ^^^ EmailId
  )
  lazy val rosmProp: Parser[RosmProp] = (
    "safeId" ^^^ RosmSafeId
      | "organisationName" ^^^ RosmOrganisationName
      | "organisationType" ^^^ RosmOrganisationType
      | "isAGroup" ^^^ RosmIsAGroup
  )

  //================= boolean parsers =========

  private lazy val p0: Parser[BooleanExpr] = "true" ^^^ IsTrue |
    "yes" ^^^ IsTrue |
    "false" ^^^ IsFalse |
    "no" ^^^ IsFalse |
    "form.phase.is.instructionPDF" ^^ { loc =>
      FormPhase(InstructionPDF)
    } |
    FormComponentId.unanchoredIdValidation ^^ { fcId =>
      TopLevelRef(BooleanExprId(fcId))
    } |
    "(" ~> p4 <~ ")"

  lazy val quoteRegexParse: Parser[Regex] = "'" ~> "[^']+".r <~ "'" ^^ { regex =>
    regex.r
  }

  private lazy val formCtxParse: Parser[FormCtx] = FormComponentId.unanchoredIdValidation ^^ { fcId =>
    FormCtx(FormComponentId(fcId))
  }

  private lazy val p1: PackratParser[BooleanExpr] = (exprFormCtx ~ "<" ~ exprFormCtx ^^ { case expr1 ~ _ ~ expr2 =>
    LessThan(expr1, expr2)
  }
    | exprFormCtx ~ "<=" ~ exprFormCtx ^^ { case expr1 ~ _ ~ expr2 =>
      LessThanOrEquals(expr1, expr2)
    }
    | exprFormCtx ~ "=" ~ exprFormCtx ^^ { case expr1 ~ _ ~ expr2 =>
      Equals(expr1, expr2)
    }
    | exprFormCtx ~ "!=" ~ exprFormCtx ^^ { case expr1 ~ _ ~ expr2 =>
      Not(Equals(expr1, expr2))
    }
    | exprFormCtx ~ ">=" ~ exprFormCtx ^^ { case expr1 ~ _ ~ expr2 =>
      GreaterThanOrEquals(expr1, expr2)
    }
    | exprFormCtx ~ ">" ~ exprFormCtx ^^ { case expr1 ~ _ ~ expr2 =>
      GreaterThan(expr1, expr2)
    }
    | formCtxParse ~ "(?i)\\Qcontains\\E".r ~ exprFormCtx ^^ { case formCtx ~ _ ~ expr =>
      Contains(formCtx, expr)
    }
    | formCtxParse ~ "match" ~ quoteRegexParse ^^ { case formCtx ~ _ ~ regex =>
      MatchRegex(formCtx, regex)
    }
    | contextField ~ "in" ~ dataSourceParse ^^ { case expr ~ _ ~ dataSource =>
      In(expr, dataSource)
    }
    | dateExpr ~ "before" ~ dateExpr ^^ { case expr1 ~ _ ~ expr2 =>
      DateBefore(expr1, expr2)
    }
    | dateExpr ~ "after" ~ dateExpr ^^ { case expr1 ~ _ ~ expr2 =>
      DateAfter(expr1, expr2)
    }
    | p0)

  private lazy val p2: PackratParser[BooleanExpr] = ("!" ~> p1 ^^ { e =>
    Not(e)
  }
    | p1)

  private lazy val p3: PackratParser[BooleanExpr] = (p3 ~ "&&" ~ p2 ^^ { case expr1 ~ _ ~ expr2 =>
    And(expr1, expr2)
  }
    | p2)

  lazy val p4: PackratParser[BooleanExpr] = (p4 ~ "||" ~ p3 ^^ { case expr1 ~ op ~ expr2 =>
    Or(expr1, expr2)
  }
    | p3)

  lazy val booleanExpr: Parser[BooleanExpr] = "${" ~ p4 ~ "}" ^^ { case _ ~ e ~ _ =>
    e
  }

  ///===============   expression parser =======

  lazy val expressionParser_expr: Parser[FormCtx] = "${" ~> FormComponentId.unanchoredIdValidation <~ "}" ^^ { field =>
    FormCtx(FormComponentId(field))
  }

  ///================   selection criteria parser =========

  lazy val alphabeticOnly2: Parser[String] = """\w+""".r ^^ { str =>
    str
  }

  lazy val selectionCriteria =
    expressionParser_expr ^^ { expr =>
      SelectionCriteriaExpr(expr)
    } | FormComponentId.unanchoredIdValidation ~ "." ~ alphabeticOnly2 ^^ { case id ~ _ ~ column =>
      SelectionCriteriaReference(FormCtx(FormComponentId(id)), CsvColumnName(column))
    } |
      alphabeticOnly ^^ { value =>
        SelectionCriteriaSimpleValue(List(value))
      }

  ///// ========     Presentation Hit parser ===========

  lazy val presentationHints: Parser[List[PresentationHint]] =
    (presentationHint ~ "," ~ presentationHints ^^ { case presHint ~ _ ~ presHints =>
      presHint :: presHints
    }
      | presentationHint ^^ { presHint =>
        List(presHint)
      })

  lazy val presentationHint: Parser[PresentationHint] = (
    "summariseGroupAsGrid" ^^^ SummariseGroupAsGrid
      | "invisibleInSummary" ^^^ InvisibleInSummary
      | "totalValue" ^^^ TotalValue
      | "invisiblePageTitle" ^^^ InvisiblePageTitle
  )

  ///// =========== Label size parser ========

  lazy val labelSize: Parser[LabelSize] =
    "xl" ^^^ ExtraLarge |
      "l" ^^^ Large |
      "m" ^^^ Medium |
      "s" ^^^ Small |
      "xs" ^^^ ExtraSmall

}

case object ValueParser extends ValueParser {

  def validate(expression: String): Opt[ValueExpr] = {
    val y = new ValueParser.PackratReader(new CharSequenceReader(expression))

    Try {
      val res = ValueParser.parse(exprDeterminer, y)
      Console.println(s"\n\n${res}\n\n")
      res.get.rewrite
    }.toEither.left.map(x => UnexpectedState(x.toString))

  }

  def validateWithParser[A](expression: String, parser: Parser[A]): Opt[A] = {
    val y = new ValueParser.PackratReader(new CharSequenceReader(expression))

    Try {
      ValueParser.parseAll(parser, y).get
    }.toEither.left.map(x => UnexpectedState(x.toString))

  }

}
