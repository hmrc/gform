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

case object CreateEnvelope
case class UploadFile(envelopeId: EnvelopeId, fileId: FileId)

trait HttpExecutor[U, P, I] {
  def makeCall(url: ServiceUrl[U], params: P, body: I)(implicit wts: Writes[I], rds: HttpReads[HttpResponse], hc: HeaderCarrier): Future[HttpResponse]
}

object HttpExecutor {
  implicit object createEnvelope extends HttpExecutor[FusUrl, CreateEnvelope.type, JsObject] {
    def makeCall(fusUrl: ServiceUrl[FusUrl], params: CreateEnvelope.type, body: JsObject)(implicit wts: Writes[JsObject], rds: HttpReads[HttpResponse], hc: HeaderCarrier): Future[HttpResponse] = {
      WSHttp.POST[JsObject, HttpResponse](s"${fusUrl.url}/file-upload/envelopes", body)
    }
  }

  implicit object uploadFile extends HttpExecutor[FusFeUrl, UploadFile, Array[Byte]] {
    def makeCall(fusFeUrl: ServiceUrl[FusFeUrl], params: UploadFile, body: Array[Byte])(implicit wts: Writes[Array[Byte]], rds: HttpReads[HttpResponse], hc: HeaderCarrier): Future[HttpResponse] = {
      import params._
      val url = s"${fusFeUrl.url}/file-upload/upload/envelopes/$envelopeId/files/$fileId"
      FusFeUploadWS.doFormPartPost(url, fileId, "application/xml; charset=UTF-8", ByteString.fromArray(body), Seq("CSRF-token" -> "nocheck"))
    }
  }

  def apply[U, P, I](
    url: ServiceUrl[U],
    params: P,
    body: I
  )(
    implicit
    hc: HeaderCarrier,
    ec: ExecutionContext,
    httpExecutor: HttpExecutor[U, P, I],
    wts: Writes[I]
  ): Future[HttpResponse] = {
    httpExecutor.makeCall(url, params, body)
  }
}
