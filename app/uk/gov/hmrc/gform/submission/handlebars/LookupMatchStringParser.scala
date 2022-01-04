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

package uk.gov.hmrc.gform.submission.handlebars

import scala.util.parsing.combinator.RegexParsers
import cats.syntax.eq._
import cats.instances.int._
import cats.instances.string._
import cats.syntax.option._

sealed trait KeyComponent extends Product with Serializable
object KeyComponent {
  case class LiteralString(s: String) extends KeyComponent
  case object Wildcard extends KeyComponent
  case object Null extends KeyComponent
}

object LookupMatchStringParser extends RegexParsers {
  override def skipWhitespace: Boolean = true

  def apply(input: String, key: List[String]): Option[String] = {
    def isMatchingCase(caseKey: List[KeyComponent]): Boolean =
      if (caseKey.size =!= key.size) false
      else
        caseKey.zip(key).forall {
          case (KeyComponent.LiteralString(s), k) => s === k
          case (KeyComponent.Null, k)             => k == null
          case (KeyComponent.Wildcard, _)         => true
        }

    def charSequenceParser: Parser[String] = "[^']*".r

    def nullParser: Parser[KeyComponent] = "null" ^^ { case _ => KeyComponent.Null }
    def wildcardParser: Parser[KeyComponent] = "*" ^^ { case _ => KeyComponent.Wildcard }
    def literalStringKeyComponentParser: Parser[KeyComponent] = quotedStringParser ^^ { case s =>
      KeyComponent.LiteralString(s)
    }

    def compositeKeyComponentParser: LookupMatchStringParser.Parser[KeyComponent] =
      literalStringKeyComponentParser | wildcardParser | nullParser

    def compositeKeyParser: Parser[List[KeyComponent]] = "(" ~ rep(compositeKeyComponentParser) ~ ")" ^^ {
      case _ ~ keyValues ~ _ => keyValues
    }

    def quotedStringParser: Parser[String] = "'" ~ charSequenceParser ~ "'" ^^ { case _ ~ charSequence ~ _ =>
      charSequence
    }

    def caseParser: Parser[Option[String]] = compositeKeyParser ~ "=>" ~ quotedStringParser ~ ";?".r ^^ {
      case caseKey ~ _ ~ resultValue ~ _ =>
        if (isMatchingCase(caseKey)) resultValue.some
        else None
    }

    def matchParser: LookupMatchStringParser.Parser[List[Option[String]]] = phrase(rep1(caseParser))

    parse(matchParser, input) match {
      case Success(result, _) => result.flatten.headOption
      case NoSuccess(msg, _)  => throw new Exception(s"Unexpected parse failure: $msg")
    }
  }
}
