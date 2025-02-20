/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.objectstore

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{ Applicative, Monad }
import org.apache.pekko.NotUsed
import org.apache.pekko.stream.Materializer
import org.apache.pekko.stream.scaladsl.Source
import org.apache.pekko.util.ByteString
import uk.gov.hmrc.gform.objectstore
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.envelope.{ EnvelopeData, EnvelopeFile }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.submission.{ PdfAndXmlSummaries, Submission }
import uk.gov.hmrc.gform.upscan.UpscanReference
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.objectstore.client
import uk.gov.hmrc.objectstore.client.{ ObjectSummaryWithMd5, Path }

trait ObjectStoreAlgebra[F[_]] {

  def createEnvelope(formTemplateId: FormTemplateId): F[EnvelopeId]

  def getFileBytes(envelopeId: EnvelopeId, fileName: String)(implicit hc: HeaderCarrier): F[ByteString]

  def uploadFile(
    envelopeId: EnvelopeId,
    fileId: FileId,
    fileName: String,
    content: ByteString,
    contentType: ContentType
  )(implicit hc: HeaderCarrier): F[ObjectSummaryWithMd5]

  def uploadFileWithDir(directory: Path.Directory, fileName: String, content: ByteString, contentType: ContentType)(
    implicit hc: HeaderCarrier
  ): F[ObjectSummaryWithMd5]

  def getEnvelope(envelopeId: EnvelopeId): F[EnvelopeData]

  def allUploadedFiles(envelopeId: EnvelopeId)(implicit F: Monad[F], hc: HeaderCarrier): F[List[UploadedFile]] =
    for {
      env  <- getEnvelope(envelopeId)
      file <- uploadedFiles(envelopeId, env)
    } yield file

  private def uploadedFiles(envelopeId: EnvelopeId, envelope: EnvelopeData)(implicit
    hc: HeaderCarrier,
    applicativeM: Applicative[F]
  ): F[List[UploadedFile]] =
    envelope.files.traverse[F, UploadedFile] { file: EnvelopeFile =>
      getFileBytes(envelopeId, file.fileName)
        .map { bytes =>
          objectstore.UploadedFile(
            File(FileId(file.fileId), FileStatus.Available, file.fileName, file.length),
            bytes
          )
        }
    }

  def deleteFile(envelopeId: EnvelopeId, fileIds: FileId)(implicit hc: HeaderCarrier): F[Unit]

  def deleteFiles(envelopeId: EnvelopeId, fileIds: Set[FileId])(implicit hc: HeaderCarrier): F[Unit]

  def deleteFile(directory: Path.Directory, fileName: String)(implicit hc: HeaderCarrier): F[Unit]

  def getFile(directory: Path.Directory, fileName: String)(implicit
    hc: HeaderCarrier
  ): F[Option[client.Object[Source[ByteString, NotUsed]]]]

  def zipFiles(envelopeId: EnvelopeId, objectStorePaths: ObjectStorePaths)(implicit
    hc: HeaderCarrier
  ): F[ObjectSummaryWithMd5]

  def deleteZipFile(envelopeId: EnvelopeId, objectStorePaths: ObjectStorePaths)(implicit hc: HeaderCarrier): F[Unit]

  def getZipFile(envelopeId: EnvelopeId, objectStorePaths: ObjectStorePaths)(implicit
    hc: HeaderCarrier,
    m: Materializer
  ): F[Option[client.Object[Source[ByteString, NotUsed]]]]

  def uploadFromUrl(
    from: java.net.URL,
    envelopeId: EnvelopeId,
    fileId: FileId,
    contentType: ContentType,
    fileName: String,
    reference: UpscanReference
  )(implicit hc: HeaderCarrier): F[ObjectSummaryWithMd5]

  def submitEnvelope(
    submission: Submission,
    summaries: PdfAndXmlSummaries,
    hmrcDms: HmrcDms,
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier): F[ObjectSummaryWithMd5]
}
