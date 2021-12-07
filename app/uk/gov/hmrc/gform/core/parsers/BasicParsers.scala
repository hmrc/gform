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

import cats.Eval
import cats.data.ReaderT
import cats.instances.either._
import cats.syntax.either._
import cats.parse.{ Parser, Parser0 }
import cats.parse.Rfc5234.{ alpha, digit }
import cats.parse.Parser.{ char, charIn, string }
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.core.parsers.BooleanExprParser.token

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

  def validateNonZeroPositiveNumber(expression: Int): Opt[Int] =
    validateWithParser(expression.toString, nonZeroPositiveInteger)

  def nextOrPreviousValue[A](string: String, fn: (Int, Int) => A): Parser[A] =
    (token(string) *> exactMonthDay).map { case (month, day) => fn(month, day) }

  lazy val exactMonthDay: Parser[(Int, Int)] = ((delimiter *> exactMonthParser) <* delimiter) ~ exactDayParser

  lazy val exactYearMonth: Parser[(Int, Int)] = (exactYearParser <* delimiter) ~ exactMonthParser <* delimiter

//  lazy val positiveIntegers: Parser[List[Int]] =
//    (positiveInteger ~ "," ~ positiveIntegers ^^ ((loc, x, _, xs) => x :: xs)
//      | positiveInteger ^^ ((loc, x) => List(x)))
  lazy val positiveIntegers: Parser[List[Int]] = ((positiveInteger <* token(",")) ~ positiveIntegers).map {
    case (x, xs) => x :: xs
  } |
    positiveInteger.map(x => List(x))

  val anyWordFormatParser = (digit | alpha | char('_')).rep.map(x => x.toList.mkString(""))

  val delimiter = Parser.charIn('-', ' ', '/', '.')
  //val delimiter = "[- /.]".r

  lazy val yearParser: Parser[Year] =
    exactYearParser.map(year => Year.Exact(year)) |
      token("YYYY").map(_ => Year.Any) |
      token("next").map(_ => Year.Next) |
      token("previous").map(_ => Year.Previous)

  lazy val monthParser: Parser[Month] =
    exactMonthParser.map(month => Month.Exact(month)) |
      token("MM").map(_ => Month.Any: Month)

  lazy val dayParser: Parser[Day] =
    exactDayParser.map(day => Day.Exact(day)) |
      token("DD").map(_ => Day.Any) |
      token("firstDay").map(_ => Day.First) |
      token("lastDay").map(_ => Day.Last)

  //lazy val exactYearParser: Parser[Int] = intParser("""(19|20)\d\d""")
  lazy val exactYearParser: Parser[Int] =
    ((string("19").map(_ => 1900) | string("20").map(_ => 2000)) ~ (digit.rep(2, 2)))
      .map { case (expr1, expr2) =>
        expr1 + expr2.toString().toInt
      }

  //lazy val exactMonthParser: Parser[Int] = intParser("""0[1-9]|1[012]""")
  lazy val exactMonthParser: Parser[Int] =
    (char('0').map(_ => 0) ~ charIn('1' to '9') | char('1').map(_ => 10) ~ charIn('1', '2', '3')).map {
      case (expr1, expr2) => expr1 + expr2.toInt
    }

  //lazy val exactDayParser: Parser[Int] = intParser("""0[1-9]|[12][0-9]|3[01]""")
  lazy val exactDayParser: Parser[Int] = (char('0').map(_ => 0) ~ digit).map { case (x, y) => x + y.toInt } |
    (charIn('1', '2').map(x => x.toInt * 10) ~ digit).map { case (x, y) => x + y.toInt } |
    (char('3').map(_ => 30) ~ charIn('0', '1').map(x => x.toInt)).map { case (x, y) => x + y }

  //lazy val positiveInteger: Parser[Int] = intParser("""\d+""")
  lazy val positiveInteger: Parser[Int] = digit.rep.map(x => x.toList.mkString("").toInt)

  //lazy val nonZeroPositiveInteger: Parser[Int] = intParser("""[1-9][0-9]*""")
  lazy val nonZeroPositiveInteger: Parser[Int] = (charIn('1' to '9') ~ digit.rep0).map { case (x, y) =>
    (x :: y).toList.mkString("").toInt
  }

  //lazy val anyInteger: Parser[Int] = intParser("""(\+|-)?\d+""")
  lazy val anyInteger: Parser[Int] = charIn('+', '-') *> digit.rep(1).map(x => x.toList.mkString("").toInt)

  //lazy val plusOrMinus: Parser[String] = """[+-]""".r ^^ { (_, plusOrMinus) =>
  lazy val plusOrMinus: Parser[String] = charIn('+', '-').map(x => x.toString)

  lazy val periodValueParser: Parser[String] = Parser.fail[String] //TODO
  /*lazy val periodValueParser: Parser[String] = {
    val periodComps = List("Y", "M", "D")
    (periodComps.combinations(1) ++ periodComps.combinations(2) ++ periodComps.combinations(3))
      .map(_.map("(\\+|-)?\\d+" + _))
      .map(s =>
        ("P" + s.mkString).r ^^ { (_, period) =>
          period
        }
      )
      .reduce(_ | _)
  }*/
}
