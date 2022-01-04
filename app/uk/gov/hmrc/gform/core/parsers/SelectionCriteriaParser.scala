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

import parseback._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ CsvColumnName, FormComponentId, FormCtx, SelectionCriteriaValue }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SelectionCriteriaValue._
import uk.gov.hmrc.gform.core.parsers.BasicParsers._

object SelectionCriteriaParser {

  def validate(expression: String): Opt[SelectionCriteriaValue] = validateWithParser(expression, selectionCriteria)

  lazy val selectionCriteria = (
    ExprParsers.expr ^^ { (_, expr) =>
      SelectionCriteriaExpr(expr)
    }
      |
      FormComponentId.unanchoredIdValidation ~ "." ~ FormatParser.alphabeticOnly ^^ { (_, id, _, column) =>
        SelectionCriteriaReference(FormCtx(FormComponentId(id)), CsvColumnName(column))
      } |
      FormatParser.alphabeticOnly ^^ { (_, value) =>
        SelectionCriteriaSimpleValue(List(value))
      }
  )
}
