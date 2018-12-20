/*
 * Copyright 2018 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.DestinationGen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ TextExpression, verifyRead }

class DestinationSpec extends Spec {
  "Destination" should "round trip derived JSON" in {
    forAll(DestinationGen.destinationGen) { obj =>
      Destination.format.reads(Destination.format.writes(obj)) should beJsSuccess(obj)
    }
  }

  it should "read custom JSON for HmrcDms" in {
    forAll(DestinationGen.hmrcDmsGen) { hmrcDms =>
      import hmrcDms._
      verifyRead[Destination](
        hmrcDms,
        s"""|{
            |  "id": "${id.id}",
            |  "${Destination.typeDiscriminatorFieldName}": "${Destination.hmrcDms}",
            |  "dmsFormId": "$dmsFormId",
            |  "customerId": ${TextExpression.format.writes(customerId)},
            |  "classificationType": "$classificationType",
            |  "businessArea": "$businessArea"
            |}""".stripMargin
      )
    }
  }
}
