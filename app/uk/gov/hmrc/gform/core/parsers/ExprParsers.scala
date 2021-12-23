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
import uk.gov.hmrc.gform.core.parsers.BasicParsers.validateWithParser
import uk.gov.hmrc.gform.core.parsers.LabelSizeParser.{ labelSize, parseAll }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormCtx, ValueExpr }

import scala.util.Try
import scala.util.parsing.combinator.{ PackratParsers, RegexParsers }

trait ExprParsers extends RegexParsers {

  val expr: Parser[FormCtx] = "${" ~> FormComponentId.unanchoredIdValidation <~ "}" ^^ { field =>
    FormCtx(FormComponentId(field))
  }
}

case object ExprParsers extends ExprParsers {

  def validateFormCtx(expression: String): Opt[FormCtx] =
    Try {
      val res = parseAll(expr, expression)
      Console.println(res)
      res.get
    }.toEither.left.map(x => UnexpectedState(x.toString))

}
