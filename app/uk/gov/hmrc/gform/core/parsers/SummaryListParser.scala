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

import uk.gov.hmrc.gform.core.Opt

import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormCtx, MiniSummaryListValue }

trait SummaryListParser extends ValueParser {
  val parser: Parser[MiniSummaryListValue] = FormComponentId.unanchoredIdValidation ^^ { field =>
    MiniSummaryListValue.MiniSummaryListReference(FormCtx(FormComponentId(field)))
  } | expr ^^ { field => MiniSummaryListValue.MiniSummaryListExpr(field.rewrite) }
}

object SummaryListParser extends SummaryListParser with ParsingHelper {
  def validate(expression: String): Opt[MiniSummaryListValue] = validateWithParser(expression, parser)
}
