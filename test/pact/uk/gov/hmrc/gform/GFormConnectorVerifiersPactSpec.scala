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

package pact.uk.gov.hmrc.gform

import com.itv.scalapact.ScalaPactVerify.{ loadFromLocal, verifyPact }
import uk.gov.hmrc.gform.Spec

class GFormConnectorVerifiersPactSpec extends Spec {

  ignore should "verify contract" in {

    //TODO remove hard cpded path
    //TODO add provider state
    verifyPact
      .withPactSource(loadFromLocal("/gform-frontend_gform.json"))
      .noSetupRequired
      .runVerificationAgainst("localhost", 9196)
  }
}
