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

package uk.gov.hmrc.bforms.connectors

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import play.api.libs.json.{ Json, JsObject, JsValue }
import scala.concurrent.{ ExecutionContext, Future }

import uk.gov.hmrc.play.audit.http.HttpAuditing
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.play.http.ws.{ WSDelete, WSPost, WSPut, WSGet }
import uk.gov.hmrc.play.config.AppName

import uk.gov.hmrc.play.http.hooks.HttpHook
import uk.gov.hmrc.play.http.ws._
import play.api.http.HeaderNames
import uk.gov.hmrc.bforms.model.EnvelopeId
import uk.gov.hmrc.bforms.exceptions.{ UnexpectedState, InvalidState }
import uk.gov.hmrc.bforms.WSHttp

class FusConnector(
    fileUploadBaseUrl: String,
    http: WSHttp.type
) {

  val EnvelopeIdExtractor = "envelopes/([\\w\\d-]+)$".r.unanchored
  val formatter = DateTimeFormatter.ofPattern("YYYY-MM-dd'T'HH:mm:ss'Z'")

  def createEnvelope(formTypeRef: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Either[UnexpectedState, EnvelopeId]] = {
    http.POST[JsValue, HttpResponse](s"$fileUploadBaseUrl/file-upload/envelopes", envelopeRequest(formTypeRef))
      .map { resp =>
        import HeaderNames._
        resp.header(LOCATION) match {
          case Some(location) => location match {
            case EnvelopeIdExtractor(envelopeId) => Right(EnvelopeId(envelopeId))
            case otherwise => Left(InvalidState(s"EnvelopeId in $LOCATION header: $location not found"))
          }
          case None => Left(InvalidState(s"Header $LOCATION not found"))
        }
      }
  }

  def envelopeRequest(formTypeRef: String): JsObject = {

    def envelopeExpiryDate(numberOfDays: Int) = LocalDateTime.now.plusDays(numberOfDays).format(formatter)

    Json.obj(
      "constraints" -> Json.obj(
        "contentTypes" -> Json.arr(
          "application/pdf",
          "image/jpeg"
        ),
        "maxItems" -> 5,
        "masSize" -> "30MB",
        "maxSizePerItem" -> "5MB"
      ),
      "callbackUrl" -> "someCallback",
      "expiryDate" -> s"${envelopeExpiryDate(7)}",
      "metadata" -> Json.obj(
        "application" -> "Digital Forms Service",
        "formTypeRef" -> s"$formTypeRef"
      )
    )
  }
}
