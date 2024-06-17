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

import org.apache.pekko.actor.Scheduler
import org.apache.pekko.util.ByteString
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.core.FutureSyntax
import uk.gov.hmrc.gform.objectstore.FUConfig
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.wshttp.{ FutureHttpResponseSyntax, WSHttp }

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.duration._

class FileUploadFrontendConnector(config: FUConfig, wSHttp: WSHttp)(implicit ex: ExecutionContext, schduler: Scheduler)
    extends FileUploadFrontendAlgebra[Future] with Retrying {
  private val logger = LoggerFactory.getLogger(getClass)

  def upload(envelopeId: EnvelopeId, fileId: FileId, fileName: String, body: ByteString, contentType: ContentType)(
    implicit hc: HeaderCarrier
  ): Future[Unit] = {

    val msg =
      s"upload, envelopeId: '${envelopeId.value}',  fileId: '${fileId.value}', fileName: '$fileName', contentType: '${contentType.value}, ${loggingHelpers
        .cleanHeaderCarrierHeader(hc)}'"
    logger.info(msg)

    val url = s"$baseUrl/file-upload/upload/envelopes/${envelopeId.value}/files/${fileId.value}"
    retry(
      wSHttp
        .POSTFile(url, fileName, body, Seq("CSRF-token" -> "nocheck"), contentType.value)
        .failWithNonSuccessStatusCodes(url),
      List(10.milliseconds, 100.milliseconds, 2.seconds),
      msg
    ).void
  }

  private lazy val baseUrl = config.fileUploadFrontendBaseUrl
}
