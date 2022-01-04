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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators

import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationPrint
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.PrintSection._

trait DestinationPrintGen {

  def formIdListGen: Gen[List[FormComponentId]] = PrimitiveGen.zeroOrMoreGen(FormComponentGen.formComponentIdGen)

  def destinationPrintPageGen: Gen[Page] =
    for {
      titleSmartString        <- SmartStringGen.smartStringGen
      instructionsSmartString <- SmartStringGen.smartStringGen
    } yield Page(titleSmartString, instructionsSmartString)

  def destinationPrintPdfGen: Gen[Pdf] =
    for {
      headerSmartString <- SmartStringGen.smartStringGen
      footerSmartString <- SmartStringGen.smartStringGen
    } yield Pdf(headerSmartString, footerSmartString)

  def destinationPrintPdfNotificationGen: Gen[PdfNotification] =
    for {
      headerSmartString <- SmartStringGen.smartStringGen
      footerSmartString <- SmartStringGen.smartStringGen
      fieldIds          <- formIdListGen
    } yield PdfNotification(headerSmartString, footerSmartString, fieldIds)

  def destinationPrintGen: Gen[DestinationPrint] =
    for {
      page            <- destinationPrintPageGen
      pdf             <- destinationPrintPdfGen
      pdfNotification <- Gen.option(destinationPrintPdfNotificationGen)
    } yield DestinationPrint(page, pdf, pdfNotification)
}

object DestinationPrintGen extends DestinationPrintGen
