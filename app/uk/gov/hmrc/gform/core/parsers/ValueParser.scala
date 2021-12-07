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
import cats.parse.Rfc5234.{ alpha, digit, sp }
import cats.parse.Parser
import cats.parse.Parser.{ char, charIn, map0, not, string }
import uk.gov.hmrc.gform.core.parsers.BooleanExprParser.token
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.BasicParsers._
import uk.gov.hmrc.gform.sharedmodel.dblookup.CollectionName
import uk.gov.hmrc.gform.sharedmodel.formtemplate.InternalLink.PageLink
import uk.gov.hmrc.gform.sharedmodel.formtemplate.UserField.Enrolment
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieveAttribute, DataRetrieveId }

object ValueParser {

  def validate(expression: String): Opt[ValueExpr] =
    validateWithParser(expression, exprDeterminer).right.map(_.rewrite)

  lazy val exprDeterminer: Parser[ValueExpr] = (dateExpression.map(expr => DateExpression(expr))
    | positiveIntegers.map(selections => ChoiceExpression(selections))
    | expr.map(expr => TextExpression(expr)))

  lazy val dateExpression: Parser[DateValue] = nextDate | lastDate | exactDate | today

  lazy val today: Parser[TodayDateValue.type] = token("today").map(_ => TodayDateValue)

  lazy val exactDate: Parser[ExactDateValue] = (exactYearParser ~ exactMonthDay).map { case (year, (month, day)) =>
    ExactDateValue(year, month, day)
  } | (exactYearMonth <* token("firstDay")).map { case (year, month) =>
    ExactDateValue(year, month, 1)
  } | (exactYearMonth <* token("lastDay")).map { case (year, month) =>
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

  lazy val dateExprExactQuoted: Parser[DateExpr] =
    ((char('\'') *> dateExprExactParser) <* char('\'')) | dateExprExactParser

  lazy val signedInt: Parser[Int] = (plusOrMinus ~ positiveInteger).map { case (plusOrMinus, i) =>
    i * (plusOrMinus match {
      case "+" => 1
      case "-" => -1
    })
  }

  lazy val signedYear: Parser[OffsetUnit] = (signedInt <* token("y")).map(year => OffsetUnit.Year(year))

  lazy val signedMonth: Parser[OffsetUnit] = (signedInt <* token("m")).map(month => OffsetUnit.Month(month))

  lazy val signedDay: Parser[OffsetUnit] = (signedInt <* token("d")).map(day => OffsetUnit.Day(day))

  private val offsets = List(signedYear, signedMonth, signedDay)

  private val perms1: List[Parser[OffsetYMD]] = offsets.map { ap =>
    ap.map(a => OffsetYMD(a))
  }

  private val perms2: Iterator[Parser[OffsetYMD]] =
    offsets.combinations(2).flatMap(_.permutations).map { case List(ap, bp) =>
      (ap ~ bp).map { case (a, b) =>
        OffsetYMD(a, b)
      }
    }

  private val perms3: Iterator[Parser[OffsetYMD]] = offsets.permutations.map { case List(ap, bp, cp) =>
    (ap ~ bp ~ cp).map { case ((a, b), c) =>
      OffsetYMD(a, b, c)
    }
  }

  private val allYMDVariations = perms1 ++ perms2 ++ perms3

  lazy val offsetYMD: Parser[OffsetYMD] = allYMDVariations.reduce(_ | _)

  lazy val dateExprTODAY: Parser[DateExpr] = token("TODAY").map(_ => DateValueExpr(TodayDateExprValue))

  lazy val dateExprTODAYOffset: Parser[DateExpr] = (dateExprTODAY ~ offsetYMD).map { case (dateExprToday, offsetYMD) =>
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
    token("service.seiss").map(_ => DataSource.SeissEligible)
      | (token("mongo.") *> alphabeticOnly).map(name => DataSource.Mongo(CollectionName(name)))
      | (token("user.enrolments.") *> enrolment).map(enrolment =>
        DataSource.Enrolment(enrolment.serviceName, enrolment.identifierName)
      )
      | (token("delegated.classic.enrolments.") *> enrolment).map(enrolment =>
        DataSource.DelegatedEnrolment(enrolment.serviceName, enrolment.identifierName)
      )
  )

  lazy val expr: Parser[Expr] = (quotedConstant
    | (token("${") *> parserExpression) <* token("}"))

  lazy val internalLinkParser: Parser[InternalLink] = (
    token("printAcknowledgementPdf").map(_ => InternalLink.printAcknowledgementPdf)
      | token("printSummaryPdf").map(_ => InternalLink.printSummaryPdf)
      | (token("newForm.") *> FormTemplateId.unanchoredIdValidationParser).map(id =>
        InternalLink.NewFormForTemplate(FormTemplateId(id))
      )
      | token("newForm").map(_ => InternalLink.newForm)
      | token("newSession").map(_ => InternalLink.newSession)
      | PageId.unanchoredIdValidationParser.map(id => PageLink(PageId(id)))
  )

  private lazy val periodFun = ((token("period(") *> dateExpr) <* token(",")) ~ dateExpr <* token(")")

  private lazy val userFieldFunc: Parser[UserFieldFunc] = token("count").map(_ => UserFieldFunc.Count) |
    nonZeroPositiveInteger.map(i => UserFieldFunc.Index(i))

  private lazy val userEnrolmentFunc: Parser[UserCtx] =
    (((token("user.") *> userFieldEnrolments) <* token(".")) ~ userFieldFunc).map { case (userField, func) =>
      UserCtx(Enrolment(userField.serviceName, userField.identifierName, Some(func)))
    }

  lazy val periodFnParser: Parser[PeriodFn] = string("sum").map(_ => PeriodFn.Sum) |
    string("totalMonths").map(_ => PeriodFn.TotalMonths) |
    string("years").map(_ => PeriodFn.Years) |
    string("months").map(_ => PeriodFn.Months) |
    string("days").map(_ => PeriodFn.Days)

  lazy val contextField: Parser[Expr] =
    userEnrolmentFunc | ((token("user.") *> userField).map(userField => UserCtx(userField))
      | token("form.submissionReference").map(_ => FormTemplateCtx(FormTemplateProp.SubmissionReference))
      | token("form.id").map(_ => FormTemplateCtx(FormTemplateProp.Id))
      | token("form.lang").map(_ => LangCtx)
      | (token("form.") *> FormComponentId.unanchoredIdValidationParser).map(fieldName =>
        FormCtx(FormComponentId(fieldName))
      )
      | (token("param.") *> alphabeticOnly).map(param => ParamCtx(QueryParam(param)))
      | (token("auth.") *> authInfo).map(authInfo => AuthCtx(authInfo))
      | (token("hmrcRosmRegistrationCheck.") *> rosmProp).map(rosmProp => HmrcRosmRegistrationCheck(rosmProp))
      | (token("link.") *> internalLinkParser).map(internalLink => LinkCtx(internalLink))
      | (((token("dataRetrieve.") *> DataRetrieveId.unanchoredIdValidationParser) <* token(
        "."
      )) ~ DataRetrieveAttribute.unanchoredIdValidationparser).map { case (dataRetrieveId, dataRetrieveAttribute) =>
        DataRetrieveCtx(DataRetrieveId(dataRetrieveId), DataRetrieveAttribute.fromExpr(dataRetrieveAttribute))
      }
      | dateExprWithoutFormCtxFieldDate.map(
        DateCtx.apply
      ) // to parse date form fields with offset or date constants i.e TODAY, 01012020 etc (with or without offset)
      | periodValueParser.map(period => PeriodValue(period))
      | ((periodFun <* string(".")) ~ periodFnParser).map { case ((dateExpr1: DateExpr, dateExpr2: DateExpr), prop) =>
        PeriodExt(
          Period(DateCtx(dateExpr1), DateCtx(dateExpr2)),
          prop
        )
      }
      | periodFun.map { case (dateExpr1, dateExpr2) =>
        Period(DateCtx(dateExpr1), DateCtx(dateExpr2))
      }
      | quotedLocalisedConstant
      | (FormComponentId.unanchoredIdValidationParser <* string(".sum")).map(value =>
        Sum(FormCtx(FormComponentId(value)))
      )
      | (FormComponentId.unanchoredIdValidationParser <* string(".count")).map(value => Count(FormComponentId(value)))
      | ((FormComponentId.unanchoredIdValidationParser <* string(".")) ~ addressDetail).map {
        case (value, addressDetail) =>
          AddressLens(FormComponentId(value), addressDetail)
      }
      | anyDigitConst
      | FormComponentId.unanchoredIdValidationParser.map(fn => FormCtx(FormComponentId(fn))))

  lazy val addressDetail: Parser[AddressDetail] =
    string("line1").map(_ => AddressDetail.Line1) |
      string("line2").map(_ => AddressDetail.Line2) |
      string("line3").map(_ => AddressDetail.Line3) |
      string("line4").map(_ => AddressDetail.Line4) |
      string("postcode").map(_ => AddressDetail.Postcode) |
      string("country").map(_ => AddressDetail.Country)

  lazy val formCtxFieldDate: Parser[DateExpr] = (string("form.") *> FormComponentId.unanchoredIdValidationParser).map {
    fieldName => DateFormCtxVar(FormCtx(FormComponentId(fieldName)))
  } | FormComponentId.unanchoredIdValidationParser.map { fn =>
    DateFormCtxVar(FormCtx(FormComponentId(fn)))
  }

  lazy val parserExpression: Parser[Expr] = ((token("(") *> addExpression) <* token(")")) | addExpression

  lazy val addExpression: Parser[Expr] = ((parserExpression <* token("+")) ~ parserExpression).map {
    case (expr1, expr2) =>
      Add(expr1, expr2)
  } | subtractionExpression

  lazy val subtractionExpression: Parser[Expr] =
    ((parserExpression <* token("-")) ~ parserExpression).map { case (expr1, expr2) =>
      Subtraction(expr1, expr2)
    } | product

  lazy val product: Parser[Expr] = ((parserExpression <* token("*")) ~ parserExpression).map { case (expr1, expr2) =>
    Multiply(expr1, expr2)
  } | ifElseParser

  lazy val ifElseParser: Parser[Expr] = {
    (((((token("if") *> BooleanExprParser.p4) <* token("then")) ~ parserExpression) <* token(
      "else"
    )) ~ parserExpression).map { case ((cond, expr1) m expr2) =>
      IfElse(cond, expr1, expr2)
    } | orElseParser
  }

  lazy val orElseParser: Parser[Expr] = ((parserExpression <* token("else")) ~ parserExpression).map {
    case (expr1, expr2) =>
      Else(expr1, expr2)
  } | contextField

  lazy val alphabeticOnly: Parser[String] =
    (Parser.ignoreCaseCharIn('a' to 'Z') ~ (digit | alpha | char('_')).rep0).map { case (x, y) =>
      (x :: y).mkString("")
    }

  lazy val quotedLocalisedConstant: Parser[Expr] = ((quotedConstant <* token(",")) ~ quotedConstant).map {
    case (en, cy) =>
      IfElse(Equals(LangCtx, Constant("en")), en, cy)
  } | quotedConstant

  lazy val quotedConstant: Parser[Expr] = anyConstant.surroundedBy(char('\'')) |
    token("''").map(_ => Constant(""))

  lazy val anyConstant = Parser.charsWhile(_ != '\'').map(Constant)

  private val signParser = charIn('+', '-').?.map(x => x.map(_.toString).getOrElse("")).with1

  lazy val anyDigitConst: Parser[Expr] =
    (signParser ~ digit.rep).map { case (x, value) =>
      Constant(x + value.toList.mkString(""))
    }

  /*
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
  )*/

  lazy val userFieldEnrolments: Parser[UserField.Enrolment] = string("enrolments.") *> enrolment

  lazy val userField: Parser[UserField] = (
    token("affinityGroup").map(_ => UserField.AffinityGroup)
      | userFieldEnrolments
      | token("enrolledIdentifier").map(_ => UserField.EnrolledIdentifier)
  )

  lazy val enrolment: Parser[UserField.Enrolment] = ((serviceName <* string(".")) ~ identifierName).map {
    case (sn, in) =>
      UserField.Enrolment(sn, in, None)
  }

  lazy val serviceName: Parser[ServiceName] = {
    val chars = Set('.', '!', '=', ' ', '}')
    Parser.charsWhile(c => !chars.contains(c)).map(ServiceName.apply)
  }

  lazy val identifierName: Parser[IdentifierName] = {
    val chars = Set('.', '!', '=', ' ', '}')
    Parser.charsWhile(c => !chars.contains(c)).map(IdentifierName.apply)
  }

  lazy val authInfo: Parser[AuthInfo] =
    token("gg").map(_ => GG) |
      token("payenino").map(_ => PayeNino) |
      token("sautr").map(_ => SaUtr) |
      token("ctutr").map(_ => CtUtr) |
      token("email").map(_ => EmailId)

  lazy val rosmProp: Parser[RosmProp] =
    token("safeId").map(_ => RosmSafeId) |
      token("organisationName").map(_ => RosmOrganisationName) |
      token("organisationType").map(_ => RosmOrganisationType) |
      token("isAGroup").map(_ => RosmIsAGroup)

}
