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

package uk.gov.hmrc.gform.objectstore

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import akka.util.ByteString
import cats.instances.list._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{ Applicative, Monad }
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.envelope.EnvelopeAlgebra
import uk.gov.hmrc.gform.fileupload.{ File, UploadedFile }
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.envelope.{ Available, EnvelopeData, EnvelopeFile }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.objectstore.client
import uk.gov.hmrc.objectstore.client.ObjectSummaryWithMd5

import scala.concurrent.{ ExecutionContext, Future }

trait ObjectStoreAlgebra[F[_]] {
  def getFileBytes(envelopeId: EnvelopeId, fileName: String)(implicit hc: HeaderCarrier): F[ByteString]

  def uploadFile(
    envelopeId: EnvelopeId,
    fileId: FileId,
    fileName: String,
    content: ByteString,
    contentType: ContentType,
    path: String = ""
  )(implicit hc: HeaderCarrier): F[ObjectSummaryWithMd5]

  def getEnvelope(envelopeId: EnvelopeId): F[EnvelopeData]

  def isObjectStore(envelopeId: EnvelopeId): F[Boolean]

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

  def deleteFile(envelopeId: EnvelopeId, fileIds: FileId)(implicit hc: HeaderCarrier): F[Unit]

  def zipFiles(envelopeId: EnvelopeId, path: String = "")(implicit hc: HeaderCarrier): F[ObjectSummaryWithMd5]

  def deleteZipFile(envelopeId: EnvelopeId, path: String = "")(implicit hc: HeaderCarrier): F[Unit]

  def getZipFile(
    envelopeId: EnvelopeId,
    path: String = ""
  )(implicit hc: HeaderCarrier, m: Materializer): F[Option[client.Object[Source[ByteString, NotUsed]]]]
}

class ObjectStoreService(objectStoreConnector: ObjectStoreConnector, envelopeService: EnvelopeAlgebra[Future])(implicit
  ec: ExecutionContext
) extends ObjectStoreAlgebra[Future] {
  private val logger = LoggerFactory.getLogger(getClass)
  override def getFileBytes(envelopeId: EnvelopeId, fileName: String)(implicit hc: HeaderCarrier): Future[ByteString] =
    objectStoreConnector.getFileBytes(envelopeId, fileName, "")

  override def uploadFile(
    envelopeId: EnvelopeId,
    fileId: FileId,
    fileName: String,
    content: ByteString,
    contentType: ContentType,
    path: String
  )(implicit hc: HeaderCarrier): Future[ObjectSummaryWithMd5] = {
    logger.info(s"upload file, envelopeId: ' ${envelopeId.value}', fileId: '${fileId.value}', fileName: '$fileName")

    for {
      envelopeData <- envelopeService.get(envelopeId)
      res <- objectStoreConnector.uploadFile(
               envelopeId,
               fileName,
               content,
               Some(contentType.value),
               path
             )
      _ <- {
        val newFiles =
          envelopeData.files :+
            EnvelopeFile(
              fileId.value,
              fileName,
              Available,
              contentType,
              res.contentLength,
              Map.empty[String, List[String]]
            )
        envelopeService.save(envelopeData.copy(files = newFiles))
      }
    } yield res
  }

  override def getEnvelope(envelopeId: EnvelopeId): Future[EnvelopeData] = envelopeService.get(envelopeId)

  override def deleteFile(envelopeId: EnvelopeId, fileId: FileId)(implicit hc: HeaderCarrier): Future[Unit] = {
    logger.info(s"delete file, envelopeId: ' ${envelopeId.value}', fileId: '$fileId'")
    for {
      envelope <- envelopeService.get(envelopeId)
      maybeFileName = envelope.files.find(_.fileId === fileId.value).map(_.fileName)
      _ <- maybeFileName match {
             case Some(fileName) => objectStoreConnector.deleteFile(envelopeId, fileName, "")
             case None           => Future.failed(new RuntimeException(s"FileId $fileId not found in mongo"))
           }
      newEnvelope = envelope.copy(files = envelope.files.filterNot(_.fileId == fileId.value))
      _ <- envelopeService.save(newEnvelope)
    } yield ()
  }

  override def zipFiles(envelopeId: EnvelopeId, path: String)(implicit
    hc: HeaderCarrier
  ): Future[ObjectSummaryWithMd5] =
    objectStoreConnector.zipFiles(envelopeId, path)

  override def deleteZipFile(envelopeId: EnvelopeId, path: String)(implicit
    hc: HeaderCarrier
  ): Future[Unit] =
    objectStoreConnector.deleteZipFile(envelopeId, path)

  override def getZipFile(
    envelopeId: EnvelopeId,
    path: String
  )(implicit hc: HeaderCarrier, m: Materializer): Future[Option[client.Object[Source[ByteString, NotUsed]]]] =
    objectStoreConnector.getZipFile(envelopeId, path)

  override def isObjectStore(envelopeId: EnvelopeId): Future[Boolean] =
    envelopeService.find(envelopeId).map(e => e.isDefined)
}
