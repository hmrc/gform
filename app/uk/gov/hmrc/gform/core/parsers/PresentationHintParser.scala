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

import cats.instances.list._
import cats.syntax.traverse._
import parseback._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.BasicParsers._
import uk.gov.hmrc.gform.models._

object PresentationHintParser {

  def validate(expression: String): Opt[PresentationHintExpr] = validateWithParser(expression, presentationHints)

  lazy val presentationHints: Parser[PresentationHints] = (
    presentationHint ~ "," ~ presentationHints ^^ { (loc, presHint, _, presHints) => PresentationHints(presHint :: presHints.hints) }
    | presentationHint ^^ { (loc, presHint) => PresentationHints(List(presHint)) }
  )

  lazy val presentationHint: Parser[PresentationHint] = (
    "collapseGroupUnderLabel" ^^ { (loc, unparsed) => CollapseGroupUnderLabel } |
    "summariseGroupAsGrid" ^^ { (loc, unparsed) => SummariseGroupAsGrid }
  )
}
