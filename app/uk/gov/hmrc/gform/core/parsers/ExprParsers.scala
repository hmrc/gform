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

package uk.gov.hmrc.gform.core.parsers

import parseback._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.BasicParsers.validateWithParser
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormCtx

object ExprParsers {

  def validateFormCtx(expression: String): Opt[FormCtx] = validateWithParser(expression, expr)

  lazy val expr: Parser[FormCtx] = "${" ~ """\w+""".r ~ "}" ^^ { (loc, _, field, _) =>
    FormCtx(field)
  }
}
