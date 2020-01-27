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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators

import org.scalacheck.Gen
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations
import uk.gov.hmrc.gform.sharedmodel.ExampleData._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.PrintSection

trait DestinationsGen {

  def destinationListGen: Gen[Destinations.DestinationList] =
    PrimitiveGen
      .oneOrMoreGen(DestinationGen.destinationGen)
      .map(Destinations.DestinationList(_, ackSection))

  def printSectionGen: Gen[Destinations.PrintSection] =
    PrimitiveGen
      .oneOrMoreGen(Gen.const(PrintSection(toSmartString("TestTitle"), toSmartString("TestSummaryPdf"))))
      .map(_.head)

  def destinationsGen: Gen[Destinations] = Gen.oneOf(destinationListGen, printSectionGen)
}

object DestinationsGen extends DestinationsGen
