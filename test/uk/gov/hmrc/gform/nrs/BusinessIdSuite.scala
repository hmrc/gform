/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.nrs

import munit.FunSuite
import play.api.libs.json._
import uk.gov.hmrc.gform.nrs.BusinessId

class BusinessIdSuite extends FunSuite {

  test("BusinessId serialisation to String") {
    val businessIdJson = Json.toJson(BusinessId("test-id"))
    assertEquals(businessIdJson, JsString("test-id"))
  }

  test("BusinessId deserialisation from String") {
    val businessId = JsString("test-id").validate[BusinessId]
    assertEquals(businessId, JsSuccess(BusinessId("test-id")))
  }
}
