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

package uk.gov.hmrc.gform.fileupload

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import play.api.http.HeaderNames.LOCATION
import play.api.libs.json.Json
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.play.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.ExecutionContext.Implicits.global

class FileUploadConnector(config: Config, wSHttp: WSHttp, timeProvider: TimeProvider) {

  def createEnvelope(formTemplateId: FormTemplateId)(implicit hc: HeaderCarrier): Future[EnvelopeId] =
    wSHttp
      .POST(s"$baseUrl/file-upload/envelopes", createEnvelopeIn(formTemplateId))
      .map(extractEnvelopId)

  def routeEnvelope(input: RouteEnvelopeRequest)(implicit hc: HeaderCarrier): Future[Unit] = {
    wSHttp
      .POST[RouteEnvelopeRequest, HttpResponse](s"$baseUrl/file-routing/requests", input)
      .map(_ => ())
  }

  /**
   * There must be Location header. If not this is exceptional situation!
   */
  private def extractEnvelopId(resp: HttpResponse): EnvelopeId = resp.header(LOCATION) match {
    case Some(EnvelopeIdExtractor(envelopeId)) => EnvelopeId(envelopeId)
    case Some(location) => throw new SpoiltLocationHeader(location)
    case _ => throw new SpoiltLocationHeader(s"Header $LOCATION not found")
  }

  private lazy val EnvelopeIdExtractor = "envelopes/([\\w\\d-]+)$".r.unanchored
  private val formatter = DateTimeFormatter.ofPattern("YYYY-MM-dd'T'HH:mm:ss'Z'")
  private def envelopeExpiryDate = timeProvider.localDateTime().plusDays(config.expiryDays.toLong).format(formatter)

  private def createEnvelopeIn(formTypeId: FormTemplateId) = Json.obj(
    "constraints" -> Json.obj(
      "contentTypes" -> Json.arr(
        "application/pdf",
        "image/jpeg"
      ),
      "maxItems" -> config.maxItems,
      "masSize" -> config.maxSize,
      "maxSizePerItem" -> config.maxSizePerItem
    ),
    "callbackUrl" -> "someCallback",
    "expiryDate" -> s"$envelopeExpiryDate",
    "metadata" -> Json.obj(
      "application" -> "gform",
      "formTypeId" -> s"${formTypeId.value}"
    )
  )

  private lazy val baseUrl = config.fileUploadBaseUrl
}
