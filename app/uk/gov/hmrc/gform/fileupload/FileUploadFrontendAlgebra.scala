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

import akka.util.ByteString
import scala.language.higherKinds
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.upscan.UploadDetails
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

trait FileUploadFrontendAlgebra[F[_]] {

  def upload(envelopeId: EnvelopeId, fileId: FileId, fileName: String, body: ByteString, contentType: ContentType)(
    implicit hc: HeaderCarrier
  ): F[Unit]

  def uploadFile(
    envelopeId: EnvelopeId,
    fileId: FileId,
    uploadDetails: UploadDetails,
    bytes: ByteString
  )(implicit hc: HeaderCarrier): F[HttpResponse]
}
