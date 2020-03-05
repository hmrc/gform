/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations

import cats.data.NonEmptyList
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.SmartString
import JsonUtils._

sealed trait Destinations extends Product with Serializable

object Destinations {

  case class DestinationList(destinations: NonEmptyList[Destination], acknowledgementSection: AcknowledgementSection)
      extends Destinations

  case class PrintSection(title: SmartString, summaryPdf: SmartString) extends Destinations

  implicit val destinationListFormat: OFormat[DestinationList] = derived.oformat

  implicit val printSectionFormat: OFormat[PrintSection] = Json.format[PrintSection]

  implicit val format: OFormat[Destinations] = {
    implicit val destinationsFormat: OFormat[Destinations] = derived.oformat

    OFormatWithTemplateReadFallback(
      safeCast[DestinationList, Destinations](destinationListFormat) orElse
        safeCast[PrintSection, Destinations](printSectionFormat)
    )
  }
}
