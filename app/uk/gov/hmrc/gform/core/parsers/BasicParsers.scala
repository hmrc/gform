/*
 * Copyright 2020 HM Revenue & Customs
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

import cats.Eval
import cats.data.ReaderT
import cats.instances.either._
import cats.syntax.either._
import parseback._
import parseback.compat.cats._
import parseback.util.Catenable
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object BasicParsers {

  private def parse[A](parser: Parser[A]) = ReaderT[Opt, String, Catenable[A]] { expression =>
    parser(LineStream[Eval](expression)).value.leftMap { error =>
      val errors: String = error.map(_.render(expression)).mkString("\n")
      UnexpectedState(s"""|Unable to parse expression $expression.
                          |Errors:
                          |$errors""".stripMargin)
    }
  }

  private def reconstruct[A](cat: Catenable[A]) = ReaderT[Opt, String, A] { expression =>
    cat.uncons match {
      case Some((expr, _)) => Right(expr)
      case None            => Left(UnexpectedState(s"Unable to parse expression $expression"))
    }
  }

  def validateWithParser[A](expression: String, parser: Parser[A]): Opt[A] =
    (for {
      catenable <- parse(parser)
      expr      <- reconstruct(catenable)
    } yield expr).run(expression.trim)

  implicit val W = Whitespace(() | """\s+""".r)

  def nextOrPreviousValue[A](string: String, fn: (Int, Int) => A): Parser[A] =
    (string ~ exactMonthDay ^^ { (loc, _, month, day) =>
      fn(month, day)
    })

  lazy val exactMonthDay: Parser[(Int, Int)] = (delimiter ~ exactMonthParser ~ delimiter ~ exactDayParser ^^ {
    (loc, _, month, _, day) =>
      (month, day)
  })

  lazy val exactYearMonth: Parser[(Int, Int)] = exactYearParser ~ delimiter ~ exactMonthParser ~ delimiter ^^ {
    (loc, year, _, month, _) =>
      (year, month)
  }

  lazy val positiveIntegers
    : Parser[List[Int]] = (positiveInteger ~ "," ~ positiveIntegers ^^ ((loc, x, _, xs) => x :: xs)
    | positiveInteger ^^ ((loc, x) => List(x)))

  val anyWordFormat = """\w+""".r
  val delimiter = "[- /.]".r

  lazy val yearParser: Parser[Year] =
    exactYearParser ^^ ((_, year) => Year.Exact(year)) |
      "YYYY" ^^ ((_, _) => Year.Any) |
      "next" ^^ ((_, _) => Year.Next) |
      "previous" ^^ ((_, _) => Year.Previous)

  lazy val monthParser: Parser[Month] =
    exactMonthParser ^^ ((_, month) => Month.Exact(month)) |
      "MM" ^^ ((_, _) => Month.Any: Month)

  lazy val dayParser: Parser[Day] =
    exactDayParser ^^ ((_, day) => Day.Exact(day)) |
      "DD" ^^ ((_, _) => Day.Any) |
      "firstDay" ^^ ((_, _) => Day.First) |
      "lastDay" ^^ ((_, _) => Day.Last)

  lazy val exactYearParser: Parser[Int] = intParser("""(19|20)\d\d""")

  lazy val exactMonthParser: Parser[Int] = intParser("""0[1-9]|1[012]""")

  lazy val exactDayParser: Parser[Int] = intParser("""0[1-9]|[12][0-9]|3[01]""")

  lazy val positiveInteger: Parser[Int] = intParser("""\d+""")

  lazy val anyInteger: Parser[Int] = intParser("""(\+|-)?\d+""")

  private def intParser(str: String): Parser[Int] =
    (str.r ^^ { (loc, number) =>
      number.toInt
    })
}
