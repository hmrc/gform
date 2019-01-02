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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, DestinationsGen }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ TextExpression, verifyRead }

class DestinationsSpec extends Spec {
  "Destinations" should "round trip derived JSON" in {
    forAll(DestinationsGen.destinationsGen) { obj =>
      Destinations.format.reads(Destinations.format.writes(obj)) should beJsSuccess(obj)
    }
  }

  it should "read template upload JSON for DestinationList" in {
    forAll(DestinationsGen.destinationListGen) { destinations =>
      val json = createJson(destinations)
      verifyRead[Destinations](destinations, json)
    }
  }

  private def createJson(destinations: Destinations.DestinationList): String = {
    val listBody = destinations.destinations
      .map {
        case hmrcDms: Destination.HmrcDms =>
          import hmrcDms._
          s"""|{
              |  "id": "${id.id}",
              |  "${Destination.typeDiscriminatorFieldName}": "${Destination.hmrcDms}",
              |  "dmsFormId": "$dmsFormId",
              |  "customerId": ${TextExpression.format.writes(customerId)},
              |  "classificationType": "$classificationType",
              |  "businessArea": "$businessArea"
              |}""".stripMargin
      }
      .toList
      .mkString(", ")

    s"[$listBody]"
  }
}
