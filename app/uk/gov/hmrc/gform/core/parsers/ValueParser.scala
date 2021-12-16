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
import cats.parse.Parser.{char, charIn, map0, not, string}
import uk.gov.hmrc.gform.core.parsers.BooleanExprParser.token
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.BasicParsers._
import uk.gov.hmrc.gform.sharedmodel.dblookup.CollectionName
import uk.gov.hmrc.gform.sharedmodel.formtemplate.InternalLink.PageLink
import uk.gov.hmrc.gform.sharedmodel.formtemplate.UserField.Enrolment
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{OffsetYMD, _}
import uk.gov.hmrc.gform.sharedmodel.{DataRetrieveAttribute, DataRetrieveId}

object ValueParser {

  val today: Parser[TodayDateValue.type] = token("today").map(_ => TodayDateValue)

  val exactDate: Parser[ExactDateValue] = (exactYearParser ~ exactMonthDay).map { case (year, (month, day)) =>
    ExactDateValue(year, month, day)
  }.backtrack | (exactYearMonth <* string("firstDay")).map { case (year, month) =>
    ExactDateValue(year, month, 1)
  }.backtrack | (exactYearMonth <* string("lastDay")).map { case (year, month) =>
    ExactDateValue(year, month, LocalDate.of(year, month, 1).lengthOfMonth)
  }

  val nextDate: Parser[NextDateValue] =
    nextOrPreviousValue("next", NextDateValue.apply)

  val lastDate: Parser[PreviousDateValue] =
    nextOrPreviousValue("last", PreviousDateValue.apply)

  val dateExpression: Parser[DateValue] = nextDate | lastDate | exactDate | today

  lazy val dateExprExactParser: Parser[DateExpr] = (exactDayParser ~ exactMonthParser ~ exactYearParser).backtrack.map {
    case ((day, month), year) => DateValueExpr(ExactDateExprValue(year, month, day))
  }

  val dateExprExactQuoted: Parser[DateExpr] =
    ((char('\'') *> dateExprExactParser) <* char('\'')) | dateExprExactParser

  lazy val signedInt: Parser[Int] = (plusOrMinus.surroundedBy(sp.rep0) ~ positiveInteger).map { case (plusOrMinus, i) =>
    i * (plusOrMinus match {
      case "+" => 1
      case "-" => -1
    })
  }

  val signedYear: Parser[OffsetUnit] = (signedInt <* string("y")).map(year => OffsetUnit.Year(year))

  val signedMonth: Parser[OffsetUnit] = (signedInt <* string("m")).map(month => OffsetUnit.Month(month))

  val signedDay: Parser[OffsetUnit] = (signedInt <* string("d")).map(day => OffsetUnit.Day(day))

  val offsets = List(signedYear, signedMonth, signedDay)

  private val perms1: List[Parser[OffsetYMD]] = offsets.map { ap =>
    ap.map(a => OffsetYMD(a))
  }

  val perms2: List[Parser[OffsetYMD]] =
    offsets.combinations(2).flatMap(_.permutations).toList.map {
      case List(ap, bp) =>
        (ap ~ bp).map { case (a, b) =>
          OffsetYMD(a, b)
        }
    }

  private val perms3: List[Parser[OffsetYMD]] = offsets.permutations.toList.map { case List(ap, bp, cp) =>
    (ap ~ bp ~ cp).map { case ((a, b), c) =>
      OffsetYMD(a, b, c)
    }
  }

  private val allYMDVariations = perms3 ++ perms2 ++ perms1

  val offsetYMD: Parser[OffsetYMD] =  Parser.oneOf(allYMDVariations.map(_.backtrack))

  val dateExprTODAY: Parser[DateExpr] = token("TODAY").map(_ => DateValueExpr(TodayDateExprValue))

  val dateExprTODAYOffset: Parser[DateExpr] = (dateExprTODAY ~ offsetYMD).map { case (dateExprToday, offsetYMD) =>
    DateExprWithOffset(dateExprToday, offsetYMD)
  }.backtrack | dateExprTODAY

  val formCtxFieldDate: Parser[DateExpr] = (string("form.") *> FormComponentId.unanchoredIdValidationParser).map {
    fieldName => DateFormCtxVar(FormCtx(FormComponentId(fieldName)))
  } | FormComponentId.unanchoredIdValidationParser.map { fn =>
    DateFormCtxVar(FormCtx(FormComponentId(fn)))
  }

  val formCtxFieldDateWithOffset: Parser[DateExprWithOffset] = (formCtxFieldDate ~ offsetYMD).map {
    case (dateExprCtx, offsetYMD) => DateExprWithOffset(dateExprCtx, offsetYMD)
  }

//  val dateExpr: Parser[DateExpr] =
//    dateExprExactQuoted | dateExprTODAYOffset | formCtxFieldDate | formCtxFieldDateWithOffset
  val dateExpr: Parser[DateExpr] =
    dateExprExactQuoted | dateExprTODAYOffset | formCtxFieldDateWithOffset | formCtxFieldDate

  val dateExprWithoutFormCtxFieldDate: Parser[DateExpr] =
    dateExprExactQuoted | dateExprTODAYOffset | formCtxFieldDateWithOffset

  val alphabeticOnly: Parser[String] =
    (Parser.ignoreCaseCharIn('a' to 'z') ~ (digit | alpha | char('_')).rep0).map { case (x, y) =>
      (x :: y).mkString("")
    }

  val identifierName: Parser[IdentifierName] = {
    val chars = Set('.', '!', '=', ' ', '}')
    Parser.charsWhile(c => !chars.contains(c)).map(IdentifierName.apply)
  }

  val serviceName: Parser[ServiceName] = {
    val chars = Set('.', '!', '=', ' ', '}')
    Parser.charsWhile(c => !chars.contains(c)).map(ServiceName.apply)
  }

  val enrolment: Parser[UserField.Enrolment] = ((serviceName <* string(".")) ~ identifierName).map { case (sn, in) =>
    UserField.Enrolment(sn, in, None)
  }

  val dataSourceParse: Parser[DataSource] = (
    token("service.seiss").map(_ => DataSource.SeissEligible)
      | (token("mongo.") *> alphabeticOnly).map(name => DataSource.Mongo(CollectionName(name)))
      | (token("user.enrolments.") *> enrolment).map(enrolment =>
        DataSource.Enrolment(enrolment.serviceName, enrolment.identifierName)
      )
      | (token("delegated.classic.enrolments.") *> enrolment).map(enrolment =>
        DataSource.DelegatedEnrolment(enrolment.serviceName, enrolment.identifierName)
      )
  )

  val internalLinkParser: Parser[InternalLink] = (
    token("printAcknowledgementPdf").map(_ => InternalLink.printAcknowledgementPdf)
      | token("printSummaryPdf").map(_ => InternalLink.printSummaryPdf)
      | (token("newForm.") *> FormTemplateId.unanchoredIdValidationParser).map(id =>
        InternalLink.NewFormForTemplate(FormTemplateId(id))
      )
      | token("newForm").map(_ => InternalLink.newForm)
      | token("newSession").map(_ => InternalLink.newSession)
      | PageId.unanchoredIdValidationParser.map(id => PageLink(PageId(id)))
  )

  private val periodFun = ((token("period(") *> dateExpr) <* token(",")) ~ dateExpr <* token(")")

  private val userFieldFunc: Parser[UserFieldFunc] = token("count").map(_ => UserFieldFunc.Count) |
    nonZeroPositiveInteger.map(i => UserFieldFunc.Index(i))

  val userFieldEnrolments: Parser[UserField.Enrolment] = string("enrolments.") *> enrolment

  private val userEnrolmentFunc: Parser[UserCtx] =
    (((token("user.") *> userFieldEnrolments) <* token(".")) ~ userFieldFunc).map { case (userField, func) =>
      UserCtx(Enrolment(userField.serviceName, userField.identifierName, Some(func)))
    }.backtrack

  val periodFnParser: Parser[PeriodFn] = string("sum").map(_ => PeriodFn.Sum) |
    string("totalMonths").map(_ => PeriodFn.TotalMonths) |
    string("years").map(_ => PeriodFn.Years) |
    string("months").map(_ => PeriodFn.Months) |
    string("days").map(_ => PeriodFn.Days)

  val rosmProp: Parser[RosmProp] =
    token("safeId").map(_ => RosmSafeId) |
      token("organisationName").map(_ => RosmOrganisationName) |
      token("organisationType").map(_ => RosmOrganisationType) |
      token("isAGroup").map(_ => RosmIsAGroup)

  val anyConstant = Parser.charsWhile(_ != '\'').map(Constant)

  val quotedConstant: Parser[Expr] = anyConstant.surroundedBy(char('\'')).backtrack |
    token("''").map(_ => Constant(""))

  val quotedLocalisedConstant: Parser[Expr] = ((quotedConstant <* token(",")) ~ quotedConstant).map { case (en, cy) =>
    IfElse(Equals(LangCtx, Constant("en")), en, cy)
  }.backtrack | quotedConstant

  private val signParser = charIn('+', '-').?.map(x => x.map(_.toString).getOrElse("")).with1
  private val signParser2 = charIn('+', '-').?.map(x => x.map(_.toString).getOrElse(""))

  val thousandSeparatorsDecimalDigit = (signParser ~ digit ~ (digit | char(',').as(',')).rep0 ~ char('.') ~ digit.rep0).map {
    case (((((sign, number1)), numbers), _), numbers2) => Constant(sign + number1.toString + numbers.mkString("") + "." + numbers2.mkString(""))
  }

  val thousandSeparatorsDigit = (signParser ~ digit ~ (digit | char(',').as(',')).rep0).map {
    case ((sign, number1), numbers) => Constant(sign + number1.toString + numbers.mkString(""))
  }

  val decimalDigit = ((signParser2 ~ digit.rep0).with1 ~ char('.') ~ digit.rep0).map {
    case (((sign, number), _), decimalDigit) => Constant(sign + number.mkString("")  + "." + decimalDigit.mkString(""))
  }

  val anyDigitConst: Parser[Expr] =
    thousandSeparatorsDecimalDigit.backtrack | thousandSeparatorsDigit.backtrack | decimalDigit.backtrack |
    (signParser ~ digit.rep).map { case (x, value) =>
      Constant(x + value.toList.mkString(""))
    }


  lazy val addressDetail: Parser[AddressDetail] =
    string("line1").map(_ => AddressDetail.Line1) |
      string("line2").map(_ => AddressDetail.Line2) |
      string("line3").map(_ => AddressDetail.Line3) |
      string("line4").map(_ => AddressDetail.Line4) |
      string("postcode").map(_ => AddressDetail.Postcode) |
      string("country").map(_ => AddressDetail.Country)

  val userField: Parser[UserField] = (
    string("affinityGroup").map(_ => UserField.AffinityGroup).backtrack
      | userFieldEnrolments.backtrack
      | string("enrolledIdentifier").map(_ => UserField.EnrolledIdentifier)
  )

  val authInfo: Parser[AuthInfo] =
      token("gg").map(_ => GG) |
      token("payenino").map(_ => PayeNino) |
      token("sautr").map(_ => SaUtr) |
      token("ctutr").map(_ => CtUtr) |
      token("email").map(_ => EmailId)

  val contextField: Parser[Expr] =
    userEnrolmentFunc | ((string("user.") *> userField).map(userField => UserCtx(userField))
      | token("form.submissionReference").map(_ => FormTemplateCtx(FormTemplateProp.SubmissionReference))
      | token("form.id").map(_ => FormTemplateCtx(FormTemplateProp.Id))
      | token("form.lang").map(_ => LangCtx)
      | dateExprWithoutFormCtxFieldDate.map(
        DateCtx.apply
      ).backtrack // to parse date form fields with offset or date constants i.e TODAY, 01012020 etc (with or without offset)
      | (string("form.") *> FormComponentId.unanchoredIdValidationParser).map(fieldName =>
        FormCtx(FormComponentId(fieldName))
      ).backtrack
      | (string("param.") *> alphabeticOnly).map(param => ParamCtx(QueryParam(param)))
      | (string("auth.") *> authInfo).map(authInfo => AuthCtx(authInfo))
      | (string("hmrcRosmRegistrationCheck.") *> rosmProp).map(rosmProp => HmrcRosmRegistrationCheck(rosmProp))
      | (string("link.") *> internalLinkParser).map(internalLink => LinkCtx(internalLink))
      | (((string("dataRetrieve.") *> DataRetrieveId.unanchoredIdValidationParser) <* token(
        "."
      )) ~ DataRetrieveAttribute.unanchoredIdValidationparser).map { case (dataRetrieveId, dataRetrieveAttribute) =>
        DataRetrieveCtx(DataRetrieveId(dataRetrieveId), DataRetrieveAttribute.fromExpr(dataRetrieveAttribute))
      }.backtrack
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
      ).backtrack
      | (FormComponentId.unanchoredIdValidationParser <* string(".count")).map(value => Count(FormComponentId(value))).backtrack
      | ((FormComponentId.unanchoredIdValidationParser <* string(".")) ~ addressDetail).map {
        case (value, addressDetail) =>
          AddressLens(FormComponentId(value), addressDetail)
      }.backtrack
      | anyDigitConst
      | FormComponentId.unanchoredIdValidationParser.map(fn => FormCtx(FormComponentId(fn))))

  def stringToken(token: String) = string(token).surroundedBy(sp.rep0).as(token)

  val combinatorExpression: Parser[Expr] = Parser.recursive[Expr] { rec: Parser[Expr] =>
    val expressionOperator = Parser.oneOf(List("+", "-", "*").map(stringToken(_).backtrack))

    val recursive = (contextField ~ expressionOperator ~ rec).map {
      case ((left, "+"), right) => Add(left, right)
      case ((left, "-"), right) => Subtraction(left, right)
      case ((left, "*"), right) => Multiply(left, right)
    }

    recursive.backtrack | contextField
  }

  val parserExpression: Parser[Expr] = ((token("(") *> combinatorExpression) <* token(")")) | combinatorExpression

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
    )) ~ parserExpression).map { case ((cond, expr1), expr2) =>
      IfElse(cond, expr1, expr2)
    } | orElseParser
  }

  lazy val orElseParser: Parser[Expr] = ((parserExpression <* token("else")) ~ parserExpression).map {
    case (expr1, expr2) =>
      Else(expr1, expr2)
  } | contextField

  lazy val exprFormCtx: Parser[Expr] = (quotedLocalisedConstant
    | parserExpression)

  val expr: Parser[Expr] = (quotedConstant
    | (token("${") *> parserExpression) <* token("}")) //

  val exprDeterminer: Parser[ValueExpr] = (dateExpression.map(expr => DateExpression(expr))
    | positiveIntegers.map(selections => ChoiceExpression(selections))
    | expr.map(expr => TextExpression(expr)))

  def validate(expression: String): Opt[ValueExpr] =
    validateWithParser(expression, exprDeterminer).right.map(_.rewrite)

}
