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

import play.api.libs.json.{ JsString, JsSuccess, Json }
import munit.FunSuite

class DevelopmentPhaseSpec extends FunSuite {

  test("DiscoveryBanner uses discovery as its banner value and JSON representation") {
    assertEquals(DiscoveryBanner.banner, "discovery")
    assertEquals(Json.toJson[DevelopmentPhase](DiscoveryBanner), JsString("discovery"))
    assertEquals(Json.fromJson[DevelopmentPhase](JsString("discovery")), JsSuccess(DiscoveryBanner))
  }

  test("research is rejected as a development phase") {
    assertEquals(Json.fromJson[DevelopmentPhase](JsString("research")).isError, true)
  }
}
