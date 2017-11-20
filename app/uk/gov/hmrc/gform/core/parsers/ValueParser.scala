/*
 * Copyright 2017 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ ChoiceExpression, DateExpression, TextExpression, ValueExpr, _ }

object ValueParser {

  case class IsSubtraction(value: Boolean)
  val isSubtraction = pureconfig.loadConfigOrThrow[IsSubtraction]("feature.subtraction").value
  def validate(expression: String): Opt[ValueExpr] = validateWithParser(expression, exprDeterminer)

  lazy val exprDeterminer: Parser[ValueExpr] = (
    dateExpression ^^ ((loc, expr) => DateExpression(expr))
    | positiveIntegers ^^ ((loc, selections) => ChoiceExpression(selections))
    | expr ^^ ((loc, expr) => TextExpression(expr)))

  lazy val dateExpression: Parser[DateValue] = (
    (nextDate | lastDate | exactDate | today) ^^ { (loc, dateExpr) => dateExpr })

  lazy val today: Parser[TodayDateValue.type] = (
    "today" ^^ { (loc, today) => TodayDateValue })

  lazy val exactDate: Parser[ExactDateValue] = (
    yearParser ~ monthDay ^^ { (loc, year, month, day) => ExactDateValue(year, month, day) })

  lazy val nextDate: Parser[NextDateValue] = nextOrPrevious("next", NextDateValue.apply)

  lazy val lastDate: Parser[PreviousDateValue] = nextOrPrevious("last", PreviousDateValue.apply)

  lazy val expr: Parser[Expr] = (
    "'" ~ anyConstant ~ "'" ^^ { (loc, _, str, _) => str }
    | "${" ~> parserExpression <~ "}")

  lazy val operation: Parser[Operation] = (
    "+" ^^ { (loc, _) => Addition }
    | "*" ^^ { (loc, _) => Multiplication })

  lazy val contextField: Parser[Expr] = (
    "eeitt" ~ "." ~ eeitt ^^ { (loc, _, _, eeitt) => EeittCtx(eeitt) }
    | "form" ~ "." ~ alphabeticOnly ^^ { (loc, _, _, fieldName) => FormCtx(fieldName) }
    | "auth" ~ "." ~ authInfo ^^ { (loc, _, _, authInfo) => AuthCtx(authInfo) }
    | alphabeticOnly ~ ".sum" ^^ { (loc, value, _) => Sum(FormCtx(value)) }
    | anyDigitConst ^^ { (loc, str) => str }
    | alphabeticOnly ^^ { (loc, fn) => FormCtx(fn) })

  lazy val parserExpression: Parser[Expr] = (
    parserExpression ~ "+" ~ parserExpression ^^ { (loc, expr1, _, expr2) => Add(expr1, expr2) }
    | parserExpression ~ "*" ~ parserExpression ^^ { (loc, expr1, _, expr2) => Multiply(expr1, expr2) }
    | subtractionFeatureSwitch)

  lazy val subtractionFeatureSwitch: Parser[Expr] =
    if (isSubtraction) {
      (parserExpression ~ "-" ~ parserExpression ^^ { (loc, expr1, _, expr2) => Subtraction(expr1, expr2) } //TODO add subtraction implementation in the frontend.
        | contextField)
    } else {
      contextField
    }

  lazy val alphabeticOnly: Parser[String] = """\w+""".r ^^ { (loc, str) => str }

  lazy val anyConstant: Parser[Constant] = (
    """[ \w,]+""".r ^^ { (loc, str) => Constant(str) })

  lazy val anyDigitConst: Parser[Expr] = (
    """[ \d,]+""".r ^^ { (loc, str) => Constant(str) })

  lazy val eeitt: Parser[Eeitt] = (
    "businessUser" ^^ { (loc, _) => BusinessUser }
    | "agent" ^^ { (loc, _) => Agent })

  lazy val authInfo: Parser[AuthInfo] = (
    "gg" ^^ { (_, _) => GG }
    | "payenino" ^^ { (_, _) => PayeNino }
    | "sautr" ^^ { (_, _) => SaUtr }
    | "ctutr" ^^ { (_, _) => CtUtr })
}
