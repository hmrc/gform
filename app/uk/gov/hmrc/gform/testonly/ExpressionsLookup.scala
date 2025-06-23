/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.testonly

import julienrf.json.derived
import play.api.libs.json.Format
import uk.gov.hmrc.gform.formtemplate.{ BooleanExprId, ExpressionId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BooleanExpr, Expr, JsonUtils }

final case class ExpressionsLookup(
  expressions: Map[ExpressionId, Expr],
  booleanExpressions: Map[BooleanExprId, BooleanExpr],
  errorMap: Map[ExpressionId, String]
)

object ExpressionsLookup {
  implicit val exprMapFormat: Format[Map[ExpressionId, Expr]] =
    JsonUtils.formatMap(ExpressionId.apply, _.id)
  implicit val booleanExprMapFormat: Format[Map[BooleanExprId, BooleanExpr]] =
    JsonUtils.formatMap(BooleanExprId.apply, _.id)
  implicit val errorMapFormat: Format[Map[ExpressionId, String]] =
    JsonUtils.formatMap(ExpressionId.apply, _.id)
  implicit val format: Format[ExpressionsLookup] = derived.oformat()
}
