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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

sealed trait ExprCardinality
case class SingleExpr(expr: Expr) extends ExprCardinality
case class MultipleExpr(fields: List[FormComponent]) extends ExprCardinality

object HasExpr {
  def unapply(fc: FormComponent): Option[ExprCardinality] = unapply(fc.`type`)

  def unapply(ct: ComponentType): Option[ExprCardinality] =
    ct match {
      case Text(_, expr, _, _)          => Some(SingleExpr(expr))
      case TextArea(_, expr, _)         => Some(SingleExpr(expr))
      case UkSortCode(expr)             => Some(SingleExpr(expr))
      case Group(fields, _, _, _, _, _) => Some(MultipleExpr(fields))
      case _                            => None
    }
}
