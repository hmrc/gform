/*
 * Copyright 2023 HM Revenue & Customs
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

class FunctionsChecker(formTemplate: FormTemplate, allExpressions: List[ExprWithPath]) {

  private val allowedCountIds: Set[FormComponentId] = formTemplate.formKind.allSections
    .collect {
      case s: Section.AddToList => Seq(s.addAnotherQuestion.id) ++ s.pages.toList.flatMap(_.fields.map(_.id)).toSet
      case s: Section.NonRepeatingPage =>
        s.page.fields.collect { case fc @ IsMultiFileUpload(fu) =>
          fc.id
        }
    }
    .flatten
    .toSet

  private val allowedIndexIds: Set[FormComponentId] = formTemplate.formKind.allSections
    .collect { case s: Section.AddToList =>
      Seq(s.addAnotherQuestion.id) ++ s.pages.toList.flatMap(_.fields.map(_.id)).toSet
    }
    .flatten
    .toSet

  private val allowedSumIds: Set[FormComponentId] = formTemplate.formKind.allSections
    .collect {
      case s: Section.NonRepeatingPage => s.page.numericFields
      case s: Section.RepeatingPage    => s.page.numericFields
      case s: Section.AddToList        => s.pages.toList.flatMap(_.numericFields)
    }
    .flatten
    .toSet

  val result: ValidationResult = allExpressions.flatMap(_.referenceInfos).foldMap {
    case ReferenceInfo.SumExpr(path, Sum(FormCtx(formComponentId))) if !allowedSumIds(formComponentId) =>
      Invalid(
        s"${path.path}: $formComponentId cannot be use with .sum function. Only numeric fields from Group component, Repeated section or AddToList section can be used with .sum function"
      )
    case ReferenceInfo.CountExpr(path, Count(formComponentId)) if !allowedCountIds(formComponentId) =>
      Invalid(
        s"${path.path}: $formComponentId cannot be use with .count function. Only MultiFile upload and AddToList id can be used with .count"
      )
    case ReferenceInfo.IndexExpr(path, Index(formComponentId)) if !allowedIndexIds(formComponentId) =>
      Invalid(
        s"${path.path}: $formComponentId cannot be use with .index function. Only AddToList id can be used with .index"
      )
    case _ => Valid
  }

}
