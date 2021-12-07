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

object PresentationHintParser {

  def validate(expression: String): Opt[List[PresentationHint]] = validateWithParser(expression, presentationHints)

  def validateSingle(expression: String): Opt[PresentationHint] = validateWithParser(expression, presentationHint)

  lazy val presentationHints: Parser[List[PresentationHint]] =
    ((presentationHint <* token(",")) ~ presentationHints).map { case (presHint, presHints) => presHint :: presHints } |
      presentationHint.map(x => List(x))

  lazy val presentationHint: Parser[PresentationHint] = token("summariseGroupAsGrid").map(_ => SummariseGroupAsGrid) |
    token("invisibleInSummary").map(_ => InvisibleInSummary)
  token("totalValue").map(x => TotalValue)
  token("invisiblePageTitle").map(x => InvisiblePageTitle)
}
