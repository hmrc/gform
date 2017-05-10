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

import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._
import parseback._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.models._
import BasicParsers._

object ValueParser {

  def validate(expression: String): Opt[ValueExpr] = validateWithParser(expression, exprDeterminer)

  def validateList(expressions: List[String]): Opt[List[ValueExpr]] =
    expressions.map(validate).sequence

  lazy val exprDeterminer: Parser[ValueExpr] = (
    dateExpression ^^ ((loc, expr) => DateExpression(expr))
    | positiveIntegers ^^ ((loc, selections) => ChoiceExpression(selections))
    | expr ^^ ((loc, expr) => TextExpression(expr))
  )

  lazy val dateExpression: Parser[DateValue] = (
    (nextDate | lastDate | exactDate | today) ^^ { (loc, dateExpr) => dateExpr }
  )

  lazy val today: Parser[TodayDateValue.type] = (
    "today" ^^ { (loc, today) => TodayDateValue }
  )

  lazy val exactDate: Parser[ExactDateValue] = (
    yearParser ~ monthDay ^^ { (loc, year, month, day) => ExactDateValue(year, month, day) }
  )

  lazy val nextDate: Parser[NextDateValue] = nextOrPrevious("next", NextDateValue.apply)

  lazy val lastDate: Parser[PreviousDateValue] = nextOrPrevious("last", PreviousDateValue.apply)

  lazy val expr: Parser[Expr] = (
    "${" ~ contextField ~ "}" ^^ { (loc, _, field, _) => field }
    | "${" ~ contextField ~ operation ~ contextField ~ "}" ^^ { (loc, _, field1, op, field2, _) =>
      op match {
        case Addition => Add(field1, field2)
        case Multiplication => Multiply(field1, field2)
      }
    }
    | anyConstant ^^ { (loc, const) => const }
  )

  lazy val operation: Parser[Operation] = (
    "+" ^^ { (loc, _) => Addition }
    | "*" ^^ { (loc, _) => Multiplication }
  )

  lazy val contextField: Parser[Expr] = (
    "eeitt" ~ "." ~ eeitt ^^ { (loc, _, _, eeitt) => EeittCtx(eeitt) }
    | "form" ~ "." ~ alphabeticOnly ^^ { (loc, _, _, fieldName) => FormCtx(fieldName) }
    | "auth" ~ "." ~ authInfo ^^ { (loc, _, _, authInfo) => AuthCtx(authInfo) }
    | alphabeticOnly ^^ { (loc, fn) => FormCtx(fn) }
  )

  lazy val alphabeticOnly: Parser[String] = """\w+""".r ^^ { (loc, str) => str }

  lazy val anyConstant: Parser[Constant] = (
    """[ \w,]+""".r ^^ { (loc, str) => Constant(str) }
  )

  lazy val eeitt: Parser[Eeitt] = (
    "businessUser" ^^ { (loc, _) => BusinessUser }
    | "agent" ^^ { (loc, _) => Agent }
  )

  lazy val authInfo: Parser[AuthInfo] = (
    "gg" ^^ { (_, _) => GG }
    | "payenino" ^^ { (_, _) => PayeNino }
    | "sautr" ^^ { (_, _) => SaUtr }
    | "ctutr" ^^ { (_, _) => CtUtr }
  )
}
