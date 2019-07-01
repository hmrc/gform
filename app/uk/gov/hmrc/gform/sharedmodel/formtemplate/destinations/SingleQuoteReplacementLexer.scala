/*
 * Copyright 2019 HM Revenue & Customs
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

object SingleQuoteReplacementLexer extends RegexParsers {
  override def skipWhitespace = false

  private def powerLiteral: Parser[String] =
    "^" ~> """[^\^]*""".r <~ "^" ^^ { s =>
      s""""$s""""
    }

  private def stringLiteral: Parser[String] = {
    val digit = s"\\d"
    val hexDigit = s"($digit|[A-Fa-f])"

    val normalCharacter = raw"""[^\\'"]""".r
    val unescapedDoubleQuote = "\"".r ^^ { _ =>
      "\\\""
    }
    val escapedSingleQuote = raw"""\\'""".r ^^ { _ =>
      "'"
    }
    val escapedNonUnicodeCharacter = raw"""\\[\\/bfnrt]""".r
    val escapedUnicodeCharacter = raw"""\\u$hexDigit{4}""".r
    val escapedCharacter: Parser[String] = escapedSingleQuote | escapedNonUnicodeCharacter | escapedUnicodeCharacter
    val character: Parser[String] = powerLiteral | normalCharacter | escapedCharacter | unescapedDoubleQuote

    "'" ~> rep(character) <~ "'" ^^ { s =>
      "\"" + s.mkString("") + "\""
    }
  }

  def nonStringLiteral: Parser[String] = raw"""[^'^]+""".r

  private def tokens: Parser[List[String]] =
    phrase(rep(powerLiteral | stringLiteral | nonStringLiteral))

  def apply(code: String): Either[String, String] =
    parse(tokens, code) match {
      case NoSuccess(msg, _)  => Left(msg)
      case Success(result, _) => Right(result.mkString(""))
    }
}
