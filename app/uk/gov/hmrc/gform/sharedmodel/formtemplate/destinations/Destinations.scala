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
import JsonUtils._

sealed trait Destinations extends Product with Serializable

object Destinations {

  case class DestinationList(
    destinations: NonEmptyList[Destination],
    acknowledgementSection: AcknowledgementSection,
    declarationSection: DeclarationSection)
      extends Destinations

  case class DestinationPrint(
    page: PrintSection.Page,
    pdf: PrintSection.Pdf,
    pdfNotification: Option[PrintSection.PdfNotification])
      extends Destinations

  implicit val destinationListFormat: OFormat[DestinationList] = derived.oformat[DestinationList]()

  implicit val destinationPrintFormat: OFormat[DestinationPrint] = Json.format[DestinationPrint]

  implicit val format: OFormat[Destinations] = {
    OFormatWithTemplateReadFallback(
      safeCast[DestinationList, Destinations](destinationListFormat) orElse
        safeCast[DestinationPrint, Destinations](destinationPrintFormat)
    )
  }
}
