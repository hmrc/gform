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

package uk.gov.hmrc.bforms.core.parsers

import cats.Eval
import cats.data.ReaderT
import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import parseback._
import parseback.compat.cats._
import parseback.util.Catenable
import uk.gov.hmrc.bforms.core._
import uk.gov.hmrc.bforms.exceptions.InvalidState
import BasicParsers._

object ValueParser {

  private def parse = ReaderT[Opt, String, Catenable[ExprDeterminer]] { expression =>
    exprDeterminer(LineStream[Eval](expression)).value.leftMap { error =>
      val errors: String = error.map(_.render(expression)).mkString("\n")
      InvalidState(
        s"""|Unable to parse expression $expression.
            |Errors:
            |$errors""".stripMargin
      )
    }
  }

  private def reconstruct(cat: Catenable[ExprDeterminer]) = ReaderT[Opt, String, ExprDeterminer] { expression =>
    cat.uncons match {
      case Some((expr, _)) => Right(expr)
      case None => Left(InvalidState(s"Unable to parse expression $expression"))
    }
  }

  def validate(expression: String): Opt[ExprDeterminer] = (for {
    catenable <- parse
    expr <- reconstruct(catenable)
  } yield expr).run(expression)

  def validateList(expressions: List[String]): Opt[List[ExprDeterminer]] =
    expressions.map(validate).sequence

  lazy val exprDeterminer: Parser[ExprDeterminer] = (
    dateExpression ^^ ((loc, expr) => DateExpression(expr))
    | choiceExpression ^^ ((loc, expr) => ChoiceExpression(expr))
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

  lazy val choiceExpression: Parser[ChoiceExpr] = (
    positiveInteger ~ "," ~ choiceExpression ^^ ((loc, x, _, xs) => ChoiceExpr(x :: xs.selections))
    | positiveInteger ^^ ((loc, x) => ChoiceExpr(List(x)))
  )

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
    context ~ "." ~ alphabeticOnly ^^ { (loc, ctx, _, fieldName) =>
      ctx match {
        case FormContext => FormCtx(fieldName)
        case AuthContext => AuthCtx(fieldName)
        case EeittContext => EeittCtx(fieldName)
      }
    }
    | alphabeticOnly ^^ { (loc, fn) => FormCtx(fn) }
  )

  lazy val alphabeticOnly: Parser[String] = """\w+""".r ^^ { (loc, str) => str }

  lazy val anyConstant: Parser[Constant] = (
    """[ \w,]+""".r ^^ { (loc, str) => Constant(str) }
  )

  lazy val context: Parser[Context] = (
    "form" ^^ { (loc, str) => FormContext }
    | "auth" ^^ { (loc, str) => AuthContext }
    | "eeitt" ^^ { (loc, str) => EeittContext }
  )

}
