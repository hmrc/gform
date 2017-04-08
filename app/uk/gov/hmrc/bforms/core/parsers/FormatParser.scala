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
import cats.syntax.either._
import parseback._
import parseback.compat.cats._
import parseback.util.Catenable
import uk.gov.hmrc.bforms.core._
import uk.gov.hmrc.bforms.exceptions.InvalidState
import BasicParsers._

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
    dateFormat
    | anyWordExpression
  )

  lazy val dateFormat: Parser[DateFormat] = (
    anyDateConstraint ^^ ((loc, constraints) => DateFormat(constraints))
    | dateConstraints ^^ ((loc, constraints) => DateFormat(constraints))
  )

  lazy val dateConstraints: Parser[DateConstraints] = (
    dateConstraint ~ "," ~ dateConstraints ^^ { (loc, x, _, xs) => DateConstraints(x :: xs.constraints) }
    | dateConstraint ^^ { (loc, x) => DateConstraints(List(x)) }
  )

  lazy val dateConstraint: Parser[DateConstraint] = (
    beforeOrAfter ~
    dateExpr ~
    offsetExpression ^^ { (loc, beforeOrAfter, dateExpr, offset) => DateConstraint(beforeOrAfter, dateExpr, offset) }
  )

  lazy val anyDateConstraint: Parser[DateConstraintType] = (
    "anyDate" ^^ { (loc, _) => AnyDate }
  )

  lazy val concreteDate: Parser[ConcreteDate] = (
    yearParser ~ monthDay ^^ { (loc, year, month, day) => ConcreteDate(year, month, day) }
  )

  lazy val nextDate: Parser[NextDate] = nextOrPrevious("next", NextDate.apply)

  lazy val previousDate: Parser[PreviousDate] = nextOrPrevious("previous", PreviousDate.apply)

  lazy val beforeOrAfter: Parser[BeforeOrAfter] = (
    "after" ^^ { (loc, after) => After }
    | "before" ^^ { (loc, before) => Before }
  )

  lazy val dateExpr: Parser[DateConstraintInfo] = (
    "today" ^^ { (loc, today) => Today }
    | concreteDate
    | nextDate
    | previousDate
    | anyWordFormat ^^ { (loc, str) => AnyWord(str) }
  )

  lazy val anyWordExpression: Parser[FormatExpr] = (
    anyWordFormat ^^ { (loc, anyWord) => TextFormat(anyWord) }
  )

  lazy val offsetExpression: Parser[OffsetDate] = (
    anyInteger ^^ { (loc, offset) => OffsetDate(offset) }
  )
}
