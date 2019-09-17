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

package uk.gov.hmrc.gform.submission

import cats.syntax.eq._
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.sharedmodel.VariadicFormData
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

case class Visibility(sections: List[Section], data: VariadicFormData, affinityGroup: Option[AffinityGroup]) {

  private def hasVisibleAncestor(formComponentId: FormComponentId): Boolean =
    sections
      .find(_.fields.exists(_.id === formComponentId))
      .forall(isVisible)

  private def areAncestorsVisible(beResultWithDep: BooleanExprResultWithDependents): Boolean =
    beResultWithDep.dependingOn.foldLeft(beResultWithDep.beResult)(_ && hasVisibleAncestor(_))

  def isVisible(section: Section): Boolean = {

    val isIncludedExpression: Option[BooleanExprResultWithDependents] =
      section.includeIf.map(incIf => BooleanExprEval.isTrue(incIf.expr, data, affinityGroup))

    isIncludedExpression.fold(true)(areAncestorsVisible)
  }
}
