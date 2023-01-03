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

import cats.Monoid
import uk.gov.hmrc.gform.core.ValidationResult
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class MutualReferenceChecker(formTemplate: FormTemplate) {
  private val validGroupReferences: ValidReferences[ReferenceKind.Group] =
    ValidReferences.group(
      formTemplate.formKind.allSections.collect {
        case s: Section.NonRepeatingPage =>
          s.page.fields.flatMap {
            case fc @ IsGroup(group) => List(fc.id -> group)
            case otherwise           => Nil
          }
        case s: Section.RepeatingPage => Nil
        case s: Section.AddToList     => Nil
      }.flatten
    )

  private val actualGroupRefs: Set[ReferenceWithPath[ReferenceKind.Group]] = formTemplate.formKind.allSections
    .collect { case s: Section.NonRepeatingPage =>
      s.page.fields.collect { case fc @ IsGroup(_) =>
        LeafExpr(TemplatePath("fields"), fc).flatMap(_.referenceInfos).collect {
          case ReferenceInfo.FormCtxExpr(path, FormCtx(fcId)) =>
            ReferenceWithPath(
              path,
              Reference(ReferenceKind.Group(fc.id), fcId)
            )
        }
      }.flatten
    }
    .flatten
    .toSet

  private val addToListRefs: Set[ReferenceWithPath[ReferenceKind.AddToList]] = formTemplate.formKind.allSections
    .collect { case a: Section.AddToList =>
      LeafExpr(TemplatePath.root, a).flatMap(_.referenceInfos).collect {
        case ReferenceInfo.FormCtxExpr(path, FormCtx(fcId)) =>
          ReferenceWithPath(path, Reference(ReferenceKind.AddToList(a.addAnotherQuestion.id), fcId))
      }
    }
    .flatten
    .toSet

  private val validAddToListReferences: ValidReferences[ReferenceKind.AddToList] = ValidReferences.addToList(
    formTemplate.formKind.allSections.collect { case s: Section.AddToList => s }
  )

  private val repeatingPageRefs: Set[ReferenceWithPath[ReferenceKind.RepeatingPage]] =
    formTemplate.formKind.allSections.zipWithIndex
      .collect { case (s: Section.RepeatingPage, index) =>
        LeafExpr(TemplatePath.root, s).flatMap(_.referenceInfos).collect {
          case ReferenceInfo.FormCtxExpr(path, FormCtx(fcId)) =>
            ReferenceWithPath(path, Reference(ReferenceKind.RepeatingPage(index), fcId))
        }
      }
      .flatten
      .toSet

  private val validRepeatedSectionReferences: ValidReferences[ReferenceKind.RepeatingPage] =
    ValidReferences.repeatingPage(formTemplate.formKind.allSections.zipWithIndex.collect {
      case (s: Section.RepeatingPage, index) =>
        (s, index)
    })

  val result: ValidationResult = Monoid.combineAll(
    List(
      validGroupReferences.check(actualGroupRefs),
      validAddToListReferences.check(addToListRefs),
      validRepeatedSectionReferences.check(repeatingPageRefs)
    )
  )

}
