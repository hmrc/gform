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

import cats.Eval
import cats.data.ReaderT
import cats.instances.either._
import cats.syntax.either._

import scala.util.parsing.combinator._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.util.Try
import scala.util.parsing.input.CharSequenceReader

trait BasicParsers extends RegexParsers {

  def nextOrPreviousValue[A](string: String, fn: (Int, Int) => A): Parser[A] =
    string ~ exactMonthDay ^^ { x =>
      fn(x._2._1, x._2._2)
    }

  lazy val exactMonthDay: Parser[(Int, Int)] = delimiter ~ exactMonthParser ~ delimiter ~ exactDayParser ^^ {
    case _ ~ month ~ _ ~ day => (month, day)
  }

  lazy val exactYearMonth: Parser[(Int, Int)] = exactYearParser ~ delimiter ~ exactMonthParser ~ delimiter ^^ {
    case year ~ _ ~ month ~ _ => (year, month)
  }

  lazy val positiveIntegers: Parser[List[Int]] =
    (positiveInteger ~ "," ~ positiveIntegers ^^ { case x ~ _ ~ xs => x :: xs }
      | positiveInteger ^^ (x => List(x)))

  val anyWordFormat = """\w+""".r
  val delimiter = "[- /.]".r

  lazy val yearParser: Parser[Year] =
    exactYearParser ^^ (year => Year.Exact(year)) |
      "YYYY" ^^^ Year.Any |
      "next" ^^^ Year.Next |
      "previous" ^^^ Year.Previous

  lazy val monthParser: Parser[Month] =
    exactMonthParser ^^ (month => Month.Exact(month)) |
      "MM" ^^^ (Month.Any: Month)

  lazy val dayParser: Parser[Day] =
    exactDayParser ^^ (day => Day.Exact(day)) |
      "DD" ^^^ Day.Any |
      "firstDay" ^^^ Day.First |
      "lastDay" ^^^ Day.Last

  lazy val exactYearParser: Parser[Int] = intParser("""(19|20)\d\d""")

  lazy val exactMonthParser: Parser[Int] = intParser("""0[1-9]|1[012]""")

  lazy val exactDayParser: Parser[Int] = intParser("""0[1-9]|[12][0-9]|3[01]""")

  lazy val positiveInteger: Parser[Int] = intParser("""\d+""")

  lazy val nonZeroPositiveInteger: Parser[Int] = intParser("""[1-9][0-9]*""")

  lazy val anyInteger: Parser[Int] = intParser("""(\+|-)?\d+""")

  lazy val plusOrMinus: Parser[String] = """[+-]""".r

  lazy val periodValueParser: Parser[String] = {
    val periodComps = List("Y", "M", "D")
    (periodComps.combinations(3) ++ periodComps.combinations(2) ++ periodComps.combinations(1))
      .map(_.map("(\\+|-)?\\d+" + _))
      .map(s =>
        ("P" + s.mkString).r ^^ { period =>
          period
        }
      )
      .reduce(_ | _)
  }

  private def intParser(str: String): Parser[Int] =
    str.r ^^ { number =>
      number.toInt
    }
}

case object BasicParsers extends BasicParsers {

  def validateWithParser[A](expression: String, parser: Parser[A]): Opt[A] =
    Try {
      val res = parseAll(parser, expression)
      Console.println(res)
      res.get
    }.toEither.left.map(x => UnexpectedState(x.toString))

  def validateNonZeroPositiveNumber(expression: Int): Opt[Int] =
    validateWithParser(expression.toString, nonZeroPositiveInteger)

}
