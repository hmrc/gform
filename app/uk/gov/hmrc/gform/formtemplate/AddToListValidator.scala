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

package uk.gov.hmrc.gform.formtemplate

import cats.implicits.catsSyntaxEq
import uk.gov.hmrc.gform.core.{ Invalid, Valid, ValidationResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class AddToListValidator(
  wrapperType: BooleanExprWrapperType,
  addToListComponentIds: List[FormComponentId]
) {
  private def validateReferences(fields: List[FormCtx]): List[ValidationResult] =
    fields
      .map(f =>
        addToListComponentIds
          .find(_ === f.formComponentId)
          .map { _ =>
            Valid
          }
          .getOrElse(
            Invalid(
              s"id '${f.formComponentId}' named in $wrapperType expression does not exist in an Add To List component"
            )
          )
      )

  def apply(boolExpr: BooleanExpr): List[ValidationResult] = boolExpr match {
    case Not(e)                     => apply(e)
    case DuplicateExists(fieldList) => validateReferences(fieldList)
    case _                          => Nil
  }

}
