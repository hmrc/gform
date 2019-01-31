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

package uk.gov.hmrc.gform.submission.handlebars

import scala.util.parsing.combinator.RegexParsers

object MagicCommasParser extends RegexParsers {
  override def skipWhitespace: Boolean = false

  private def anyCharacter: Parser[String] =
    """(?s).""".r ^^ { _.toString }

  def literalString: Parser[String] =
    """"((\\["\\])|([^"]))*"""".r ^^ { s =>
      s.toString
    }

  private def commaWithBracket: Parser[String] =
    "," ~ "(?s)\\s*".r ~ "]" ^^ {
      case _ ~ ws ~ _ =>
        s"$ws]"
    }

  private def tokenParser: Parser[String] =
    literalString |
      commaWithBracket |
      anyCharacter

  private def tokensParser: Parser[String] =
    phrase(rep(tokenParser)).map(_.mkString(""))

  def apply(input: String): String =
    parse(tokensParser, input) match {
      case Success(result, _) => result
      case NoSuccess(msg, _)  => throw new Exception(s"Unexpected parse failure: $msg")
    }

}
