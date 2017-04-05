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

package uk.gov.hmrc.bforms.core

import cats.Eval
import cats.data.ReaderT
import cats.instances.either._
import cats.syntax.either._
import parseback._
import parseback.compat.cats._
import parseback.util.Catenable
import uk.gov.hmrc.bforms.exceptions.InvalidState

/**
 * Created by dimitra on 03/04/17.
 */
object FormatParser {
  private def parse = ReaderT[Opt, String, Catenable[Format]] { formatExpr =>
    expr(LineStream[Eval](formatExpr)).value.leftMap { error =>
      val errors: String = error.map(_.render(formatExpr)).mkString("\n")
      InvalidState(
        s"""|Unable to parse format expression $formatExpr.
            |Errors:
            |$errors""".stripMargin
      )
    }
  }

  private def reconstruct(cat: Catenable[Format]) = ReaderT[Opt, String, Format] { expression =>
    cat.uncons match {
      case Some((expr, _)) => Right(expr)
      case None => Left(InvalidState(s"Unable to parse format expression $expression"))
    }
  }

  def validate(expression: String): Opt[Format] = (for {
    catenable <- parse
    expr <- reconstruct(catenable)
  } yield expr).run(expression)

  lazy val expr: Parser[Format] = (
    anyDate ^^ { (loc, anyDateExpr) => anyDateExpr }
    | (after | before) ~
    whiteSpace ~
    (today | anyDate |
      dateNext | datePrevious | anyWord) ~
      whiteSpace ~
      offset ^^ { (loc, beforeOrAfter, _, anyDateExpr, _, k) => DateExpression(beforeOrAfter + whiteSpace + anyDateExpr.value + whiteSpace + k.value) }
      | anyWord ^^ { (loc, anyFormat) => anyFormat }
  )

  val whiteSpace = " "
  val after = """after"""
  val before = """before"""
  val todayFormat = """today"""

  val anyDateFormat = """(19|20)\d\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])"""
  val dateNextFormat = """next[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])"""
  val datePreviousFormat = """previous[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])"""

  lazy val today: Parser[DateExpression] = todayFormat.r ^^ { (loc, str) => DateExpression(str) }
  lazy val anyDate: Parser[DateExpression] = anyDateFormat.r ^^ { (loc, str) => DateExpression(str) }
  lazy val dateNext: Parser[DateExpression] = dateNextFormat.r ^^ { (loc, str) => DateExpression(str) }
  lazy val datePrevious: Parser[DateExpression] = datePreviousFormat.r ^^ { (loc, str) => DateExpression(str) }

  val anyWord: Parser[DateExpression] = """\w+""".r ^^ { (loc, str) => DateExpression(str) }
  val offset: Parser[DateExpression] = """(\+|-)?\d+$""".r ^^ { (loc, str) => DateExpression(str) }
}
