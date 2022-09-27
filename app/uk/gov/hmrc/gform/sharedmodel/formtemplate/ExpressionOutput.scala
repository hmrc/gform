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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import play.api.libs.json.{ Format, OFormat, Reads }
import uk.gov.hmrc.gform.formtemplate.ExpressionId

final case class ExpressionOutput(lookup: Map[ExpressionId, Expr])

object ExpressionOutput extends JsonUtils {

  // We don't need real template Reads, because 'expressionsOutput' field is computed in post processing phase
  private val templateReads: Reads[ExpressionOutput] = implicitly[Reads[List[String]]].map { expressionIds =>
    val expressionIdsMap: Map[ExpressionId, Expr] = expressionIds.map { element =>
      ExpressionId(element) -> Constant("")
    }.toMap
    ExpressionOutput(expressionIdsMap)
  } //pure(ExpressionOutput(Map.empty))

  implicit val mapReads: Format[Map[ExpressionId, Expr]] =
    JsonUtils.formatMap[ExpressionId, Expr](ExpressionId.apply, _.id)

  implicit val format: OFormat[ExpressionOutput] = OFormatWithTemplateReadFallback(templateReads)
}
