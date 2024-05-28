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

import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

sealed trait ReferenceKind

object ReferenceKind {
  final case class Group(fc: FormComponentId) extends ReferenceKind
  final case class AddToList(fc: FormComponentId) extends ReferenceKind
  final case class RepeatingPage(index: Int)
      extends ReferenceKind // Repeated section has no explicit identifier, so let's use its index instead

  object Group {
    implicit val referenceKindDescriptor: ReferenceKindDescriptor[Group] = new ReferenceKindDescriptor[Group] {
      def describe: String = "Group component"
    }
  }
  object AddToList {
    implicit val referenceKindDescriptor: ReferenceKindDescriptor[AddToList] = new ReferenceKindDescriptor[AddToList] {
      def describe: String = "Add To List section"
    }
  }

  object RepeatingPage {
    implicit val referenceKindDescriptor: ReferenceKindDescriptor[RepeatingPage] =
      new ReferenceKindDescriptor[RepeatingPage] {
        def describe: String = "Repeated section"
      }
  }
}

sealed trait ReferenceKindDescriptor[A] {
  def describe: String
}
