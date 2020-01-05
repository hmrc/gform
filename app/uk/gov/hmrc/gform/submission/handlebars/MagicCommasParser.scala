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

package uk.gov.hmrc.gform.submission.handlebars

import scala.util.parsing.combinator.RegexParsers

object MagicCommasParser extends RegexParsers {
  override def skipWhitespace: Boolean = false

  private val anyCharacter: Parser[String] =
    """(?s).""".r ^^ { _.toString }

  private val literalString: Parser[String] =
    """"((\\["\\])|([^"]))*"""".r ^^ { s =>
      s.toString
    }

  private val commaWithBracket: Parser[String] =
    "," ~ "(?s)\\s*".r ~ "]|}".r ^^ {
      case _ ~ ws ~ bracket =>
        s"$ws$bracket"
    }

  private val tokenParser: Parser[String] =
    literalString |
      commaWithBracket |
      anyCharacter

  private val tokensParser: Parser[String] =
    phrase(rep(tokenParser)).map(_.mkString(""))

  def apply(input: String): String =
    parse(tokensParser, input) match {
      case Success(result, _) => result
      case NoSuccess(msg, _)  => throw new Exception(s"Unexpected parse failure: $msg")
    }

}
