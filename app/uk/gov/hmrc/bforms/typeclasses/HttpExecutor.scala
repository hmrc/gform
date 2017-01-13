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

import akka.util.ByteString
import play.api.libs.json.{ JsObject, JsValue, Json, Writes }
import scala.concurrent.ExecutionContext
import uk.gov.hmrc.bforms.{ WSHttp, FusFeUploadWS }
import uk.gov.hmrc.bforms.model.{ EnvelopeId, FileId }
import uk.gov.hmrc.play.http.{ HeaderCarrier, HttpReads, HttpResponse }
import scala.concurrent.Future

trait GetBody[O, T] {
  def apply(obj: O): T
}

object GetBody {
  implicit object createEnvelopBody extends GetBody[CreateEnvelope, JsObject] {
    def apply(obj: CreateEnvelope) = obj.body
  }

  implicit object uploadFileBody extends GetBody[UploadFile, Array[Byte]] {
    def apply(obj: UploadFile) = obj.body
  }

  implicit object routeEnvelopeBody extends GetBody[RouteEnvelopeRequest, RouteEnvelopeRequest] {
    def apply(obj: RouteEnvelopeRequest) = obj
  }
}

case class CreateEnvelope(body: JsObject)
case class UploadFile(envelopeId: EnvelopeId, fileId: FileId, fileName: String, contentType: String, body: Array[Byte])
case class RouteEnvelopeRequest(envelopeId: EnvelopeId, application: String, destination: String)

object RouteEnvelopeRequest {
  implicit val format = Json.format[RouteEnvelopeRequest]
}

trait HttpExecutor[U, P, I] {
  def makeCall(
    url: ServiceUrl[U],
    obj: P
  )(
    implicit
    hc: HeaderCarrier,
    wts: Writes[I],
    rds: HttpReads[HttpResponse],
    getBody: GetBody[P, I]
  ): Future[HttpResponse]
}

object HttpExecutor {
  implicit object createEnvelope extends HttpExecutor[FusUrl, CreateEnvelope, JsObject] {
    def makeCall(
      fusUrl: ServiceUrl[FusUrl],
      obj: CreateEnvelope
    )(
      implicit
      hc: HeaderCarrier,
      wts: Writes[JsObject],
      rds: HttpReads[HttpResponse],
      getBody: GetBody[CreateEnvelope, JsObject]
    ): Future[HttpResponse] = {
      WSHttp.POST[JsObject, HttpResponse](s"${fusUrl.url}/file-upload/envelopes", getBody(obj))
    }
  }

  implicit object uploadFile extends HttpExecutor[FusFeUrl, UploadFile, Array[Byte]] {
    def makeCall(
      fusFeUrl: ServiceUrl[FusFeUrl],
      obj: UploadFile
    )(
      implicit
      hc: HeaderCarrier,
      wts: Writes[Array[Byte]],
      rds: HttpReads[HttpResponse],
      getBody: GetBody[UploadFile, Array[Byte]]
    ): Future[HttpResponse] = {
      import obj._
      val url = s"${fusFeUrl.url}/file-upload/upload/envelopes/$envelopeId/files/$fileId"
      FusFeUploadWS.doFormPartPost(url, fileName, contentType, ByteString.fromArray(getBody(obj)), Seq("CSRF-token" -> "nocheck"))
    }
  }

  implicit object routeRequest extends HttpExecutor[FusUrl, RouteEnvelopeRequest, RouteEnvelopeRequest] {
    def makeCall(
      fusUrl: ServiceUrl[FusUrl],
      obj: RouteEnvelopeRequest
    )(
      implicit
      hc: HeaderCarrier,
      wts: Writes[RouteEnvelopeRequest],
      rds: HttpReads[HttpResponse],
      getBody: GetBody[RouteEnvelopeRequest, RouteEnvelopeRequest]
    ): Future[HttpResponse] = {
      WSHttp.POST[RouteEnvelopeRequest, HttpResponse](s"${fusUrl.url}/file-routing/requests", getBody(obj))
    }
  }

  def apply[U, P, I](
    url: ServiceUrl[U],
    obj: P
  )(
    implicit
    hc: HeaderCarrier,
    ec: ExecutionContext,
    httpExecutor: HttpExecutor[U, P, I],
    wts: Writes[I],
    getBody: GetBody[P, I]
  ): Future[HttpResponse] = {
    httpExecutor.makeCall(url, obj)
  }
}
