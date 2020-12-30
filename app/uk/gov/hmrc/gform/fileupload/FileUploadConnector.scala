/*
 * Copyright 2021 HM Revenue & Customs
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
import akka.util.ByteString
import org.slf4j.LoggerFactory
import play.api.libs.json.JsObject

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.commons.HttpFunctions
import uk.gov.hmrc.gform.core.FutureSyntax
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.gform.wshttp.{ FutureHttpResponseSyntax, WSHttp }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads, HttpReadsInstances, HttpResponse }

class FileUploadConnector(config: FUConfig, wSHttp: WSHttp, timeProvider: TimeProvider)(implicit ex: ExecutionContext)
    extends HttpFunctions {
  private val logger = LoggerFactory.getLogger(getClass)

  val helper = new Helper(config)
  def createEnvelope(formTemplateId: FormTemplateId, expiryDate: LocalDateTime)(
    implicit hc: HeaderCarrier): Future[EnvelopeId] = {
    logger.info(
      s"creating envelope, formTemplateId: '${formTemplateId.value}', ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")
    val requestBody = helper.createEnvelopeRequestBody(formTemplateId, expiryDate)
    implicit val httpReads: HttpReads[HttpResponse] = jsonHttpReads(HttpReadsInstances.readRaw)
    wSHttp
      .POST[JsObject, HttpResponse](s"$baseUrl/file-upload/envelopes", requestBody, headers)
      .map(helper.extractEnvelopId)
  }

  def routeEnvelope(input: RouteEnvelopeRequest)(implicit hc: HeaderCarrier): Future[Unit] = {
    logger.info(s"route envelope, input: '${input.envelopeId.value}, ${loggingHelpers.cleanHeaderCarrierHeader(hc)} ")
    val url = s"$baseUrl/file-routing/requests"
    implicit val httpReads: HttpReads[HttpResponse] = jsonHttpReads(HttpReadsInstances.readRaw)
    wSHttp
      .POST[RouteEnvelopeRequest, HttpResponse](url, input, headers)
      .failWithNonSuccessStatusCodes(url)
      .void
  }

  def getEnvelope(envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): Future[Envelope] = {
    logger.info(s"get envelope, envelopeId: '${envelopeId.value}', ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")
    implicit val httpReads: HttpReads[Envelope] = jsonHttpReads(HttpReadsInstances.readJsValue.map(_.as[Envelope]))
    wSHttp.GET[Envelope](s"$baseUrl/file-upload/envelopes/${envelopeId.value}")
  }

  def getFileBytes(envelopeId: EnvelopeId, fileId: FileId)(implicit hc: HeaderCarrier): Future[ByteString] = {
    logger.info(s"get file, envelopeId: '${envelopeId.value}, fileId: '${fileId.value}'', ${loggingHelpers
      .cleanHeaderCarrierHeader(hc)}")

    val url = s"$baseUrl/file-upload/envelopes/${envelopeId.value}/files/${fileId.value}/content"
    wSHttp
      .buildRequest(url)
      .get
      .flatMap { response =>
        if (response.status < 200 || response.status > 299)
          Future.failed(new Exception(s"Got status code ${response.status} when trying to get $url"))
        else
          Future.successful(response.bodyAsBytes)
      }
  }

  def deleteFile(envelopeId: EnvelopeId, fileId: FileId)(implicit hc: HeaderCarrier): Future[Unit] = {
    logger.info(s"delete file, envelopeId: ' ${envelopeId.value}', fileId: '${fileId.value}', ${loggingHelpers
      .cleanHeaderCarrierHeader(hc)}")
    val url = s"$baseUrl/file-upload/envelopes/${envelopeId.value}/files/${fileId.value}"
    implicit val httpReads: HttpReads[HttpResponse] = jsonHttpReads(HttpReadsInstances.readRaw)
    wSHttp
      .DELETE[HttpResponse](url)
      .failWithNonSuccessStatusCodes(url)
      .void
  }
  private lazy val baseUrl = config.fileUploadBaseUrl
  private lazy val `Csrf-Token: nocheck` = "Csrf-Token" -> "nocheck"

  /**
    * TIP. The Crsf-Token is not needed on production. It's as well not intrusive.
    * We're adding it here in order to be able to call FU service using GFORM test-only proxy endpoints.
    */
  private lazy val headers = Seq(`Csrf-Token: nocheck`)
}
