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

package uk.gov.hmrc.gform.typeclasses

import akka.util.ByteString
import play.api.libs.json._

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.{ FusFeUploadWS, WSHttp }
import uk.gov.hmrc.gform.models.{ CreateEnvelope, RouteEnvelopeRequest, UploadFile }
import uk.gov.hmrc.play.http.{ HeaderCarrier, HttpResponse }

trait Post[I, O] {
  def apply(input: I): Future[O]
}

object Post {

  implicit def createEnvelope(implicit fusUrl: ServiceUrl[FusUrl], hc: HeaderCarrier) = new Post[CreateEnvelope, HttpResponse] {
    def apply(input: CreateEnvelope): Future[HttpResponse] = {
      WSHttp.POST[JsObject, HttpResponse](fusUrl.url + "/file-upload/envelopes", input.value)
    }
  }

  implicit def routeEnvelope(implicit fusUrl: ServiceUrl[FusUrl], hc: HeaderCarrier) = new Post[RouteEnvelopeRequest, HttpResponse] {
    def apply(input: RouteEnvelopeRequest): Future[HttpResponse] = {
      WSHttp.POST[RouteEnvelopeRequest, HttpResponse](fusUrl.url + "/file-routing/requests", input)
    }
  }

  implicit def uploadFileToFus(implicit fusFeUrl: ServiceUrl[FusFeUrl], hc: HeaderCarrier) = new Post[UploadFile, HttpResponse] {
    def apply(input: UploadFile): Future[HttpResponse] = {
      import input._
      FusFeUploadWS.doFormPartPost(s"${fusFeUrl.url}/file-upload/upload/envelopes/$envelopeId/files/$fileId", fileName, contentType, ByteString.fromArray(body), Seq("CSRF-token" -> "nocheck"))
    }
  }
}
