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
import java.time.LocalDateTime
import akka.util.ByteString
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{ Applicative, Monad }
import uk.gov.hmrc.gform.dms.FileAttachment
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AllowedFileTypes, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.submission.{ PdfAndXmlSummaries, Submission }
import uk.gov.hmrc.http.HeaderCarrier

trait FileDownloadAlgebra[F[_]] {
  def getEnvelope(envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): F[Envelope]

  def getFileBytes(envelopeId: EnvelopeId, fileId: FileId)(implicit hc: HeaderCarrier): F[ByteString]

  def allUploadedFiles(envelopeId: EnvelopeId)(implicit hc: HeaderCarrier, F: Monad[F]): F[List[UploadedFile]] =
    for {
      env  <- getEnvelope(envelopeId)
      file <- uploadedFiles(envelopeId, env)
    } yield file

  private def uploadedFiles(envelopeId: EnvelopeId, envelope: Envelope)(implicit
    hc: HeaderCarrier,
    applicativeM: Applicative[F]
  ): F[List[UploadedFile]] =
    envelope.files.traverse[F, UploadedFile] { file: File =>
      getFileBytes(envelopeId, file.fileId)
        .map(bytes => UploadedFile(file, bytes))
    }
}

trait FileUploadAlgebra[F[_]] {
  def createEnvelope(
    formTemplateId: FormTemplateId,
    allowedFileTypes: AllowedFileTypes,
    expiryDate: LocalDateTime,
    fileSizeLimit: Option[Int],
    objectStore: Boolean
  )(implicit
    hc: HeaderCarrier
  ): F[EnvelopeId]

  def submitEnvelope(submission: Submission, summaries: PdfAndXmlSummaries, hmrcDms: HmrcDms, objectStore: Boolean)(
    implicit hc: HeaderCarrier
  ): F[Unit]

  def deleteFile(envelopeId: EnvelopeId, fileId: FileId)(implicit hc: HeaderCarrier): F[Unit]

  def uploadAttachment(envelopeId: EnvelopeId, fileAttachment: FileAttachment, objectStore: Boolean)(implicit
    hc: HeaderCarrier
  ): F[Unit]
}
