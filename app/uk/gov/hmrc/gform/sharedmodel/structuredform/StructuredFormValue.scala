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

package uk.gov.hmrc.gform.sharedmodel.structuredform

import play.api.libs.json.{ Format, OFormat }
import julienrf.json.derived

sealed trait StructuredFormValue extends Product with Serializable

object StructuredFormValue {
  case class ObjectStructure(fields: List[Field]) extends StructuredFormValue
  case class TextNode(value: String) extends StructuredFormValue
  case class ArrayNode(elements: List[StructuredFormValue]) extends StructuredFormValue

  implicit val oFormat: Format[StructuredFormValue] = derived.oformat[StructuredFormValue]
  implicit val objectStructureFormat: OFormat[ObjectStructure] = derived.oformat[ObjectStructure]
}
