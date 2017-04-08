/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.models

import julienrf.json.derived
import play.api.libs.json._

sealed trait ComponentTypeRaw

case object TextRaw extends ComponentTypeRaw

case object DateRaw extends ComponentTypeRaw

case object AddressRaw extends ComponentTypeRaw

case object ChoiceRaw extends ComponentTypeRaw

object ComponentTypeRaw {

  val componentMap: Map[String, ComponentTypeRaw] =
    Map(
      "text" -> TextRaw,
      "date" -> DateRaw,
      "address" -> AddressRaw,
      "choice" -> ChoiceRaw
    )

  implicit val format: OFormat[ComponentTypeRaw] = {

    val format: OFormat[ComponentTypeRaw] = derived.oformat

    val reads: Reads[ComponentTypeRaw] = Reads {

      case JsString(compTypeAsString) =>

        componentMap.get(compTypeAsString) match {
          case Some(componentType) => JsSuccess(componentType)
          case None => JsError(s"Expected one of the following types: ${componentMap.values}, you entered: $compTypeAsString")
        }

      case otherwise => JsError(s"Expected String as JsValue, got: $otherwise")
    }

    OFormat[ComponentTypeRaw](reads, format)
  }
}
