/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations

import scala.util.parsing.combinator.RegexParsers
import cats.instances.string._
import cats.syntax.eq._

object HandlebarsJsonValidator extends RegexParsers {
  private def void[T]: PartialFunction[T, Unit] = { case _ => () }

  private def stringLiteral(quote: String): Parser[Unit] = {
    val escapedEscape = raw"""\\\\""".r ^^ void
    val escapedQuote = raw"""\\\$quote""".r ^^ void

    val character =
      escapedEscape |
        escapedQuote |
        handlebarsTopLevelExpression |
        raw"""[^$quote]""".r ^^ void

    quote ~ rep(character) ~ quote ^^ void
  }

  private val doubleQuotedStringLiteral = stringLiteral("\"")
  private val singleQuotedStringLiteral = stringLiteral("'")

  private def identifier: Parser[String] = "[a-zA-Z0-9_][a-zA-Z0-9_-]*".r

  private def handlebarsParameter: Parser[Unit] =
    handlebarsSubExpression |
      handlebarsIdentifier ^^ void |
      doubleQuotedStringLiteral |
      singleQuotedStringLiteral

  private def handlebarsParameterList: Parser[Unit] =
    handlebarsParameter ~ handlebarsParameterList ^^ void |
      success(())

  private def handlebarsIdentifier: Parser[Unit] =
    "." ^^ void |
      "@?[a-zA-Z0-9_][.a-zA-Z0-9_-]*".r ^^ void

  private def handlebarsExpression: Parser[Unit] =
    handlebarsIdentifier ~ handlebarsParameterList ^^ void

  private def handlebarsSubExpression: Parser[Unit] = "(" ~ handlebarsExpression ~ ")" ^^ void
  private def handlebarsTopLevelExpression: Parser[Unit] = "{{" ~ handlebarsExpression ~ "}}" ^^ void

  private def handlebarsBlockStart: Parser[String] = "{{#" ~> identifier <~ handlebarsParameterList <~ "}}"
  private def handlebarsBlockEnd: Parser[String] = "{{/" ~> identifier <~ "}}"
  private def handlebarsBlockContentElement: Parser[Unit] =
    jsonField |
      handlebarsJson

  private def handlebarsBlock: Parser[Unit] =
    handlebarsBlockStart ~ rep(handlebarsBlockContentElement ~ opt(",")) ~ handlebarsBlockEnd flatMap {
      case startId ~ _ ~ endId =>
        if (startId === endId) success(()) else err(s"Expected {{/$startId}}. Got {{/$endId}}.")
    }

  private def jsonField: Parser[Unit] =
    doubleQuotedStringLiteral ~ ":" ~ handlebarsJson ^^ void |
      handlebarsBlock |
      handlebarsTopLevelExpression

  private def jsonFieldList: Parser[Unit] =
    jsonField ~ opt(",") ~ jsonFieldList ^^ void |
      jsonField ^^ void |
      success(())

  private def jsonObject: Parser[Unit] = "{" ~ jsonFieldList ~ "}" ^^ void

  private def jsonArray: Parser[Unit] =
    "[" ~ "]" ^^ void |
      "[" ~ rep(handlebarsJson ~ opt(",")) ~ "]" ^^ void

  private def numericLiteral: Parser[Unit] = """((-)?(\d)|([1-9]\d*))(\.\d+)?([Ee][+-]?\d+)?""".r ^^ void
  private def booleanLiteral: Parser[Unit] = "(true)|(false)".r ^^ void
  private def nullLiteral: Parser[Unit] = "null" ^^ void

  private def jsonLiteral: Parser[Unit] =
    doubleQuotedStringLiteral |
      numericLiteral |
      booleanLiteral |
      nullLiteral

  private def handlebarsJson: Parser[Unit] =
    handlebarsBlock |
      handlebarsTopLevelExpression |
      jsonObject |
      jsonArray |
      jsonLiteral

  def apply(code: String): Either[String, String] =
    parse(phrase(handlebarsJson), code) match {
      case NoSuccess(msg, next) => Left(s"Line ${next.pos.line}, Column ${next.pos.column}: $msg")
      case Success(_, _)        => Right(code)
      case Error(msg, _)        => Left(msg)
      case Failure(msg, _)      => Left(msg)
    }

}
