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

package uk.gov.hmrc.gform.fileUpload

import java.time.LocalDateTime

import akka.util.ByteString
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class FileUploadFrontendConnector(config: Config, wSHttp: WSHttp)(implicit ec: ExecutionContext) {

  def upload(envelopeId: EnvelopeId, fileId: FileId, body: ByteString, contentType: ContentType)(implicit hc: HeaderCarrier): Future[Unit] =
    wSHttp
      .POSTFile(
        s"$baseUrl/file-upload/upload/envelopes/${envelopeId.value}/files/${fileId.value}",
        fileId.value,
        body,
        Seq("CSRF-token" -> "nocheck"),
        contentType.value
      )
      .map(_ => ())

  private lazy val baseUrl = config.fileUploadFrontendBaseUrl

}
