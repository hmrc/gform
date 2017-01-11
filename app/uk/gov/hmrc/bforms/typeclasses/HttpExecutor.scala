/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.typeclasses

import play.api.libs.json.{ JsObject, JsValue, Json, Writes }
import uk.gov.hmrc.bforms.WSHttp
import uk.gov.hmrc.play.http.{ HeaderCarrier, HttpReads, HttpResponse }
import scala.concurrent.Future

trait CreateEnvelope

trait HttpExecutor[T, I, O] {
  def makeCall(body: I)(implicit wts: Writes[I], rds: HttpReads[O], hc: HeaderCarrier): Future[O]
}

object HttpExecutor {
  implicit def createEnvelope(implicit fusUrl: ServiceUrl[FusUrl]) = new HttpExecutor[CreateEnvelope, JsValue, HttpResponse] {
    def makeCall(body: JsValue)(implicit wts: Writes[JsValue], rds: HttpReads[HttpResponse], hc: HeaderCarrier): Future[HttpResponse] = {
      WSHttp.POST[JsValue, HttpResponse](s"${fusUrl.url}/file-upload/envelopes", body)
    }
  }
}
