/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import julienrf.json.derived
import play.api.libs.json._

sealed trait ComponentTypeRaw

case object TextRaw extends ComponentTypeRaw

case object DateRaw extends ComponentTypeRaw

case object CalendarDateRaw extends ComponentTypeRaw

case object TaxPeriodDateRaw extends ComponentTypeRaw

case object AddressRaw extends ComponentTypeRaw

case object ChoiceRaw extends ComponentTypeRaw

case object RevealingChoiceRaw extends ComponentTypeRaw

case object HmrcTaxPeriodRaw extends ComponentTypeRaw

case object FileUploadRaw extends ComponentTypeRaw

case object GroupRaw extends ComponentTypeRaw

case object InfoRaw extends ComponentTypeRaw

case object TimeRaw extends ComponentTypeRaw

case object OverseasAddressRaw extends ComponentTypeRaw

case object PostcodeLookupRaw extends ComponentTypeRaw

case object SummaryListRaw extends ComponentTypeRaw

object ComponentTypeRaw {

  val componentMap: Map[String, ComponentTypeRaw] =
    Map(
      "text"            -> TextRaw,
      "date"            -> DateRaw,
      "calendarDate"    -> CalendarDateRaw,
      "taxPeriodDate"   -> TaxPeriodDateRaw,
      "address"         -> AddressRaw,
      "choice"          -> ChoiceRaw,
      "revealingChoice" -> RevealingChoiceRaw,
      "group"           -> GroupRaw,
      "file"            -> FileUploadRaw,
      "info"            -> InfoRaw,
      "hmrcTaxPeriod"   -> HmrcTaxPeriodRaw,
      "time"            -> TimeRaw,
      "overseasAddress" -> OverseasAddressRaw,
      "postcodeLookup"  -> PostcodeLookupRaw,
      "miniSummaryList" -> SummaryListRaw,
      ""                -> TextRaw
    )

  implicit val format: OFormat[ComponentTypeRaw] = {

    val format: OFormat[ComponentTypeRaw] = derived.oformat()

    val reads: Reads[ComponentTypeRaw] = Reads {

      case JsString(compTypeAsString) =>
        componentMap.get(compTypeAsString) match {
          case Some(componentType) => JsSuccess(componentType)
          case None =>
            val componentNames = componentMap.keys.filterNot(_.isEmpty).mkString(", ")
            throw new Exception(
              JsError(
                s"Unknown component type: $compTypeAsString. Expected one of the following: $componentNames"
              ).toString
            )
        }

      case otherwise => JsError(s"Expected String as JsValue, got: $otherwise")
    }

    OFormat[ComponentTypeRaw](reads, format)
  }
}
