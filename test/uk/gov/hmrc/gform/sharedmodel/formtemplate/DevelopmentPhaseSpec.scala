/*
 * Copyright 2023 HM Revenue & Customs
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

import play.api.libs.json.{ JsString, Json }
import uk.gov.hmrc.gform.Spec

class DevelopmentPhaseSpec extends Spec {

  "DiscoveryBanner" should "use discovery as its banner value and JSON representation" in {
    DiscoveryBanner.banner shouldBe "discovery"
    Json.toJson[DevelopmentPhase](DiscoveryBanner) shouldBe JsString("discovery")
    Json.fromJson[DevelopmentPhase](JsString("discovery")) shouldBe play.api.libs.json.JsSuccess(DiscoveryBanner)
  }

  it should "reject research as a development phase" in {
    Json.fromJson[DevelopmentPhase](JsString("research")).isError shouldBe true
  }
}
