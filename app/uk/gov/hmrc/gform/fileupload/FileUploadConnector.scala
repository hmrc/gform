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

package uk.gov.hmrc.gform.fileupload

import org.apache.pekko.http.scaladsl.model.StatusCodes
import org.apache.pekko.stream.Materializer
import org.apache.pekko.util.ByteString
import org.slf4j.LoggerFactory
import play.api.libs.json.Json
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.core.FutureSyntax
import uk.gov.hmrc.gform.envelope.EnvelopeAlgebra
import uk.gov.hmrc.gform.objectstore.{ Envelope, FUConfig, RouteEnvelopeRequest }
import uk.gov.hmrc.gform.sharedmodel.envelope.EnvelopeData
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AllowedFileTypes, FormTemplateId }
import uk.gov.hmrc.gform.wshttp.FutureHttpResponseSyntax
import uk.gov.hmrc.http.HttpReads.Implicits.readFromJson
import uk.gov.hmrc.http._
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw
import uk.gov.hmrc.http.client.HttpClientV2

import java.time.LocalDateTime
import scala.concurrent.{ ExecutionContext, Future }

class FileUploadConnector(
  config: FUConfig,
  httpClient: HttpClientV2,
  envelopeService: EnvelopeAlgebra[Future]
)(implicit ex: ExecutionContext) {
  private val logger = LoggerFactory.getLogger(getClass)

  val helper = new Helper(config)
  def createEnvelope(
    formTemplateId: FormTemplateId,
    allowedFileTypes: AllowedFileTypes,
    expiryDate: LocalDateTime,
    fileSizeLimit: Option[Int],
    objectStore: Boolean
  )(implicit
    hc: HeaderCarrier
  ): Future[EnvelopeId] = {
    logger.info(
      s"creating envelope, formTemplateId: '${formTemplateId.value}', ${loggingHelpers.cleanHeaderCarrierHeader(hc)}"
    )

    if (objectStore) {
      val newEnvelope = EnvelopeData.createEnvelope
      for {
        _   <- envelopeService.save(newEnvelope)
        res <- Future.successful(newEnvelope._id)
      } yield res
    } else {

      val requestBody = helper.createEnvelopeRequestBody(formTemplateId, allowedFileTypes, expiryDate, fileSizeLimit)

      val url = s"$baseUrl/file-upload/envelopes"

      httpClient
        .post(url"$url")
        .withBody(requestBody)
        .setHeader(headers)
        .execute[HttpResponse]
        .flatMap { response =>
          val status = response.status
          if (status == StatusCodes.Created.intValue) {
            Future.successful(helper.extractEnvelopId(response))
          } else {
            Future.failed(
              new Exception(s"POST to $url failed with status $status. Response body: '${response.body}'")
            )
          }
        } recoverWith { ex =>
        Future.failed(new Exception(s"POST to $url failed. $ex"))
      }
    }
  }

  def routeEnvelope(input: RouteEnvelopeRequest)(implicit hc: HeaderCarrier): Future[Unit] = {
    logger.info(s"route envelope, input: '${input.envelopeId.value}, ${loggingHelpers.cleanHeaderCarrierHeader(hc)} ")
    val url = s"$baseUrl/file-routing/requests"
    httpClient
      .post(url"$url")
      .withBody(Json.toJson(input))
      .setHeader(headers)
      .execute[HttpResponse]
      .failWithNonSuccessStatusCodes(url)
      .void
  }

  def getEnvelope(envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): Future[Envelope] = {
    logger.info(s"get envelope, envelopeId: '${envelopeId.value}', ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")
    httpClient
      .get(url"$baseUrl/file-upload/envelopes/${envelopeId.value}")
      .execute[Envelope]
  }

  def getFileBytes(envelopeId: EnvelopeId, fileId: FileId)(implicit
    hc: HeaderCarrier,
    mat: Materializer
  ): Future[ByteString] = {
    logger.info(s"get file, envelopeId: '${envelopeId.value}, fileId: '${fileId.value}'', ${loggingHelpers
      .cleanHeaderCarrierHeader(hc)}")

    val url = s"$baseUrl/file-upload/envelopes/${envelopeId.value}/files/${fileId.value}/content"
    httpClient
      .get(url"$url")
      .stream[HttpResponse]
      .flatMap { response =>
        if (response.status < 200 || response.status > 299)
          Future.failed(new Exception(s"Got status code ${response.status} when trying to get $url"))
        else
          response.bodyAsSource.runFold(ByteString.empty)(_ ++ _)
      }
  }

  def deleteFile(envelopeId: EnvelopeId, fileId: FileId)(implicit hc: HeaderCarrier): Future[Unit] = {
    logger.info(s"delete file, envelopeId: ' ${envelopeId.value}', fileId: '${fileId.value}', ${loggingHelpers
      .cleanHeaderCarrierHeader(hc)}")
    val url = s"$baseUrl/file-upload/envelopes/${envelopeId.value}/files/${fileId.value}"
    httpClient
      .delete(url"$url")
      .execute[HttpResponse]
      .failWithNonSuccessStatusCodes(url)
      .void
  }
  private lazy val baseUrl = config.fileUploadBaseUrl

  /** TIP. The Crsf-Token is not needed on production. It's as well not intrusive.
    * We're adding it here in order to be able to call FU service using GFORM test-only proxy endpoints.
    */
  private lazy val headers: (String, String) = "Csrf-Token" -> "nocheck"
}
