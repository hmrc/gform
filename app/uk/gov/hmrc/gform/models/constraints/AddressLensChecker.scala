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

package uk.gov.hmrc.gform.models.constraints

import cats.implicits._
import uk.gov.hmrc.gform.core.{ Invalid, Valid, ValidationResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class AddressLensChecker(formTemplate: FormTemplate) {

  private val allAddressIds: Set[FormComponentId] = formTemplate.formComponents {
    case fc @ (IsAddress(_) | IsOverseasAddress(_)) =>
      fc.id
  }.toSet

  private val allExpressions: List[ExprWithPath] = LeafExpr(TemplatePath.root, formTemplate)

  val result: ValidationResult =
    allExpressions.flatMap(_.referenceInfos).foldMap {
      case ReferenceInfo.AddressExpr(path, AddressLens(formComponentId, detail)) if !allAddressIds(formComponentId) =>
        Invalid(
          s"${path.path}: $formComponentId cannot be use with .${detail.functionName} function. $formComponentId is not an Address or OverseasAddress component"
        )
      case _ => Valid
    }

}
