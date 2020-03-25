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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationPrint
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.PrintSection.{ Page, Pdf }

trait DestinationsGen {

  def destinationListGen: Gen[Destinations.DestinationList] =
    PrimitiveGen
      .oneOrMoreGen(DestinationGen.destinationGen)
      .map(Destinations.DestinationList(_, ackSection, decSection))

  def destinationPrintGen: Gen[DestinationPrint] =
    for {
      pdf <- Gen.option(
              Pdf(
                toSmartString("TestHeader"),
                toSmartString("TestFooter"),
                List(FormComponentId("TestId1"), FormComponentId("TestId2"))))

      destinationPrint <- PrimitiveGen
                           .oneOrMoreGen(
                             Gen.const(
                               DestinationPrint(
                                 Page(toSmartString("TestTitle"), toSmartString("TestInstructions")),
                                 pdf)))
                           .map(_.head)
    } yield destinationPrint

  def destinationsGen: Gen[Destinations] = Gen.oneOf(destinationListGen, destinationPrintGen)
}

object DestinationsGen extends DestinationsGen
