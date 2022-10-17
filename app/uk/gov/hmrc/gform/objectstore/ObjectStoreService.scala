/*
 * Copyright 2022 HM Revenue & Customs
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

import akka.util.ByteString
import cats.syntax.traverse._
import cats.instances.list._
import cats.{ Applicative, Monad }
import cats.syntax.functor._
import cats.syntax.flatMap._
import uk.gov.hmrc.gform.envelope.{ EnvelopeAlgebra, EnvelopeData, EnvelopeFile }
import uk.gov.hmrc.gform.fileupload.{ File, UploadedFile }
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.objectstore.client.ObjectSummaryWithMd5

import scala.concurrent.{ ExecutionContext, Future }

trait ObjectStoreAlgebra[F[_]] {
  def getFileBytes(envelopeId: EnvelopeId, fileName: String)(implicit hc: HeaderCarrier): F[ByteString]

  def uploadFile(
    envelopeId: EnvelopeId,
    fileId: FileId,
    fileName: String,
    content: ByteString,
    contentType: ContentType
  )(implicit hc: HeaderCarrier): F[ObjectSummaryWithMd5]

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
          UploadedFile(
            File(FileId(file.fileId), uk.gov.hmrc.gform.fileupload.Available, file.fileName, file.length),
            bytes
          )
        }
    }

}

class ObjectStoreService(objectStoreConnector: ObjectStoreConnector, envelopeService: EnvelopeAlgebra[Future])(implicit
  ec: ExecutionContext
) extends ObjectStoreAlgebra[Future] {
  override def getFileBytes(envelopeId: EnvelopeId, fileName: String)(implicit hc: HeaderCarrier): Future[ByteString] =
    objectStoreConnector.getFileBytes(envelopeId, fileName)

  override def uploadFile(
    envelopeId: EnvelopeId,
    fileId: FileId,
    fileName: String,
    content: ByteString,
    contentType: ContentType
  )(implicit hc: HeaderCarrier): Future[ObjectSummaryWithMd5] =
    for {
      envelopeData <- envelopeService.get(envelopeId)
      res <- objectStoreConnector.uploadFile(
               envelopeId,
               fileName,
               content,
               Some(contentType.value)
             )
      _ <- {
        val newFiles =
          envelopeData.files :+
            EnvelopeFile(
              fileId.value,
              fileName,
              uk.gov.hmrc.gform.envelope.Available,
              contentType,
              res.contentLength,
              Map.empty[String, List[String]]
            )
        envelopeService.save(envelopeData.copy(files = newFiles))
      }
    } yield res

  override def getEnvelope(envelopeId: EnvelopeId): Future[EnvelopeData] = envelopeService.get(envelopeId)
}
