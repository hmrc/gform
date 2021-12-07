/*
 * Copyright 2021 HM Revenue & Customs
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

import cats.parse.Parser
import uk.gov.hmrc.gform.core.parsers.BooleanExprParser.token
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.BasicParsers._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object LabelSizeParser {

  def validate(expression: String): Opt[LabelSize] = validateWithParser(expression, labelSize)

  lazy val labelSize: Parser[LabelSize] =
    token("xl").map(_ => ExtraLarge) |
      token("l").map(_ => Large) |
      token("m").map(_ => Medium) |
      token("s").map(_ => Small) |
      token("xs").map(_ => ExtraSmall)

}
