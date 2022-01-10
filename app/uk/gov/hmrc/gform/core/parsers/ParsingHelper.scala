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

import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.UnexpectedState

import scala.util.Try
import scala.util.parsing.combinator.{ PackratParsers, RegexParsers }
import scala.util.parsing.input.CharSequenceReader

trait ParsingHelper extends RegexParsers {

  def validateWithParser[A](expression: String, parser: Parser[A]): Opt[A] =
    parseAll(parser, expression) match {
      case Success(result, _) => Right(result)
      case Failure(msg, _) =>
        Left(UnexpectedState(s"Unable to parse expression $expression.\nErrors:\n$msg"))
      case Error(msg, _) =>
        Left(UnexpectedState(s"Unable to parse expression $expression.\nErrors:\n$msg"))
    }

}

trait PackratParsingHelper extends RegexParsers with PackratParsers {

  def validateWithParser[A](expression: String, parser: Parser[A]): Opt[A] = {
    val input = new PackratReader(new CharSequenceReader(expression))
    parseAll(parser, input) match {
      case Success(result, _) => Right(result)
      case Failure(msg, _) =>
        Left(UnexpectedState(s"Unable to parse expression $expression.\nErrors:\n$msg"))
      case Error(msg, _) =>
        Left(UnexpectedState(s"Unable to parse expression $expression.\nErrors:\n$msg"))
    }
  }

}
