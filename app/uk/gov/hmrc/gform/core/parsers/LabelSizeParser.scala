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

import scala.util.parsing.combinator._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.PresentationHintParser.parseAll
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.util.Try

object LabelSizeParser extends RegexParsers {

  val labelSize: Parser[LabelSize] =
    "xl" ^^^ ExtraLarge |
      "l" ^^^ Large |
      "m" ^^^ Medium |
      "s" ^^^ Small |
      "xs" ^^^ ExtraSmall

  def validate(expression: String): Opt[LabelSize] =
    Try {
      val res = parseAll(labelSize, expression)
      Console.println(res)
      res.get
    }.toEither.left.map(x => UnexpectedState(x.toString))

}
