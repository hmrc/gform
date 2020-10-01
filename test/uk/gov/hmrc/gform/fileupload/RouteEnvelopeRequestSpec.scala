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

package uk.gov.hmrc.gform.fileupload

import play.api.libs.json.{ JsObject, JsSuccess, Json }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.ExampleData

class RouteEnvelopeRequestSpec extends Spec {

  behavior of "RouteEnvelopeRequest"

  it should "render json" in new ExampleData {

    val jsObject =
      Json.obj("envelopeId" -> "b66c5979-e885-49cd-9281-c7f42ce6b307", "application" -> "dfs", "destination" -> "DMS")

    val writtenJsObject: JsObject = RouteEnvelopeRequest.format.writes(routeEnvelopeRequest)
    jsObject shouldBe writtenJsObject
    RouteEnvelopeRequest.format.reads(jsObject) should be(JsSuccess(routeEnvelopeRequest))
  }

}
