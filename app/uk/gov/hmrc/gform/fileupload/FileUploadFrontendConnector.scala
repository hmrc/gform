/*
 * Copyright 2018 HM Revenue & Customs
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

import akka.util.ByteString
import play.api.Logger
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.wshttp.WSHttp

import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import scala.concurrent.Future
import uk.gov.hmrc.http.HeaderCarrier

class FileUploadFrontendConnector(config: FUConfig, wSHttp: WSHttp) {

  def upload(envelopeId: EnvelopeId, fileId: FileId, fileName: String, body: ByteString, contentType: ContentType)(implicit hc: HeaderCarrier): Future[Unit] = {
    Logger.info(s"upload, envelopeId: '${envelopeId.value}',  fileId: '${fileId.value}', fileName: '${fileName}', contentType: '${contentType.value}, ${loggingHelpers.cleanHeaderCarrierHeader(hc)}'")
    wSHttp
      .POSTFile(
        s"$baseUrl/file-upload/upload/envelopes/${envelopeId.value}/files/${fileId.value}",
        fileName,
        body,
        Seq("CSRF-token" -> "nocheck"),
        contentType.value)
      .map(_ => ())
  }
  private lazy val baseUrl = config.fileUploadFrontendBaseUrl

}
