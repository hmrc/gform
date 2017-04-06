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
  private def parse = ReaderT[Opt, String, Catenable[FormatExpr]] { formatExpr =>
    expr(LineStream[Eval](formatExpr)).value.leftMap { error =>
      val errors: String = error.map(_.render(formatExpr)).mkString("\n")
      InvalidState(
        s"""|Unable to parse format expression $formatExpr.
            |Errors:
            |$errors""".stripMargin
      )
    }
  }

  private def reconstruct(cat: Catenable[FormatExpr]) = ReaderT[Opt, String, FormatExpr] { expression =>
    cat.uncons match {
      case Some((expr, _)) => Right(expr)
      case None => Left(InvalidState(s"Unable to parse format expression $expression"))
    }
  }

  def validate(expression: String): Opt[FormatExpr] = (for {
    catenable <- parse
    expr <- reconstruct(catenable)
  } yield expr).run(expression)

  lazy val expr: Parser[FormatExpr] = (
    generalDateExpression
    | dateExpression
    | anyWordExpression
  )

  lazy val dateExpression: Parser[DateExpression] = (
    beforeOrAfter ~
    whiteSpace ~
    dateExpr ~
    whiteSpace ~
    offsetExpression ^^ { (loc, beforeOrAfter, _, dateExpr, _, offset) => DateExpression(beforeOrAfter, dateExpr, offset) }
  )

  lazy val generalDateExpression: Parser[FormatExpr] = (
    anyDateFormat.r ^^ { (loc, generalDate) => GeneralDate }
  )

  lazy val beforeOrAfter: Parser[BeforeOrAfter] = (
    "after" ^^ { (loc, after) => After }
    | "before" ^^ { (loc, before) => Before }
  )

  val splitBy = (dateAsStr: String) => dateAsStr.split("-")

  val splitAnyDate = (dateAsStr: String) => AnyDate(splitBy(dateAsStr)(0).toInt, splitBy(dateAsStr)(1).toInt, splitBy(dateAsStr)(2).toInt)
  val splitNextDate = (dateAsStr: String) => NextDate(splitBy(dateAsStr)(1).toInt, splitBy(dateAsStr)(2).toInt)
  val splitPreviousDate = (dateAsStr: String) => PreviousDate(splitBy(dateAsStr)(1).toInt, splitBy(dateAsStr)(2).toInt)

  lazy val dateExpr: Parser[DateFormat] = (
    "today" ^^ { (loc, today) => Today }
    | anyDateFormat.r ^^ { (loc, anyDate) => splitAnyDate(anyDate) }
    | dateNextFormat.r ^^ { (loc, nextDate) => splitNextDate(nextDate) }
    | datePreviousFormat.r ^^ { (loc, previousDate) => splitPreviousDate(previousDate) }
    | anyWordFormat.r ^^ { (loc, str) => AnyWord(str) }
  )

  lazy val anyWordExpression: Parser[FormatExpr] = (
    anyWordFormat.r ^^ { (loc, anyWord) => AnyOtherWord }
  )

  lazy val offsetExpression: Parser[OffsetDate] = (
    offsetFormat.r ^^ { (loc, offset) => OffsetDate(offset.toInt) }
  )

  val whiteSpace = " "
  val anyWordFormat = """\w+"""
  val offsetFormat = """(\+|-)?\d+$"""

  val anyDateFormat = """(19|20)\d\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])"""
  val dateNextFormat = """next[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])"""
  val datePreviousFormat = """previous[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])"""

}
