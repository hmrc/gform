/*
 * Copyright 2017 HM Revenue & Customs
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

import parseback.{ Parser, _ }
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.BasicParsers.{ validateWithParser, _ }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object ExprParsers {

  def validate(expression: String): Opt[FormCtx] = validateWithParser(expression, expr)

  lazy val expr: Parser[FormCtx] = "$" ~ contextField ~ "" ^^ { (loc, _, field, _) => field }

  lazy val contextField: Parser[FormCtx] = alphabeticOnly ^^ { (loc, fn) => FormCtx(fn) }

  lazy val alphabeticOnly: Parser[String] = """\w+""".r ^^ { (loc, str) => str }

}
