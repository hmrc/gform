/*
 * Copyright 2025 HM Revenue & Customs
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

import uk.gov.hmrc.gform.sharedmodel.formtemplate._

sealed trait Sugar

object Sugar {
  case class SugarExpr(e: Expr) extends Sugar
  case class SugarAnd(l: Sugar, r: Sugar) extends Sugar
  case class SugarOr(l: Sugar, r: Sugar) extends Sugar

  def buildContains(formCtx: FormCtx, sugarExpr: Sugar): BooleanExpr = {
    def loop(s: Sugar): BooleanExpr =
      s match {
        case SugarOr(left, right)  => Or(loop(left), loop(right))
        case SugarAnd(left, right) => And(loop(left), loop(right))
        case SugarExpr(expr)       => Contains(formCtx, expr)
      }
    loop(sugarExpr)
  }
}
