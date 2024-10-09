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

import uk.gov.hmrc.gform.core.{ Invalid, Valid, ValidationResult }
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, ExprWithPath, FormCtx, FormTemplate }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.models.constraints.ReferenceInfo.FormCtxExpr

object AcknowledgementValidator {
  def validateNoPIITitle(formTemplate: FormTemplate, allExpressions: List[ExprWithPath]): ValidationResult = {
    val exprRefs = allExpressions.flatMap(x =>
      x.referenceInfos.collect {
        case FormCtxExpr(_, FormCtx(fcId)) if !FormTemplateValidator.noPIIFcIds(formTemplate).contains(fcId) =>
          x.expr
      }
    )
    def hasPIIReference(title: SmartString) = {
      val exprs: List[Expr] = title.internals.flatMap(_.interpolations)
      exprRefs.exists(exprs.contains)
    }

    formTemplate.destinations match {
      case destinationList: DestinationList
          if destinationList.acknowledgementSection.title.exists(hasPIIReference) &&
            destinationList.acknowledgementSection.noPIITitle.isEmpty =>
        Invalid(
          s"The acknowledgement section does not have the property 'notPII': true and there is no noPIITitle defined"
        )
      case _ => Valid
    }
  }

}
