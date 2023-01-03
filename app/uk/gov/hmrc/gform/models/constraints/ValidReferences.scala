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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, Group, Section }

class ValidReferences[A <: ReferenceKind] private (refs: Set[Reference[A]]) {
  private val fcIds: Set[FormComponentId] = refs.map(_.formComponentId)

  private def isInvalid(addToListReference: Reference[A]): Boolean =
    fcIds(addToListReference.formComponentId) && // Reference must exists
      !refs(addToListReference) // And it must be be exact match

  def check(references: Set[ReferenceWithPath[A]])(implicit descriptor: ReferenceKindDescriptor[A]): ValidationResult =
    references.toList.foldMap { ref =>
      val invalid = isInvalid(ref.reference)
      if (invalid)
        Invalid(s"${ref.path.path}: ${ref.reference.formComponentId} belongs to different " + descriptor.describe)
      else Valid
    }
}

object ValidReferences {
  def addToList(xs: List[Section.AddToList]): ValidReferences[ReferenceKind.AddToList] =
    new ValidReferences(
      xs.flatMap { s =>
        val addToListScopeId = s.addAnotherQuestion.id
        val fcIds = s.pages.toList.flatMap(_.allFormComponentIds).toSet
        fcIds.map(fcId => Reference(ReferenceKind.AddToList(addToListScopeId), fcId))
      }.toSet
    )

  def group(xs: List[(FormComponentId, Group)]): ValidReferences[ReferenceKind.Group] =
    new ValidReferences(
      xs.flatMap { case (scopeId, group) =>
        group.fields.map { field =>
          Reference(ReferenceKind.Group(scopeId), field.id)
        }
      }.toSet
    )

  def repeatingPage(xs: List[(Section.RepeatingPage, Int)]): ValidReferences[ReferenceKind.RepeatingPage] =
    new ValidReferences(
      xs.flatMap { case (repeatingPage, index) =>
        repeatingPage.page.fields.map { field =>
          Reference(ReferenceKind.RepeatingPage(index), field.id)
        }
      }.toSet
    )

}
