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

package uk.gov.hmrc.gform.envelope

import akka.util.ByteString
import cats.{ Applicative, Monad }
import cats.instances.future._
import cats.syntax.functor._
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.fileupload.{ File, UploadedFile }
import uk.gov.hmrc.gform.objectstore.ObjectStoreConnector
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

trait EnvelopeAlgebra[F[_]] {
  def save(envelope: EnvelopeData): F[Unit]

  def get(envelopeId: EnvelopeId): F[EnvelopeData]

  def getFileBytes(envelopeId: EnvelopeId, fileName: String)(implicit hc: HeaderCarrier): F[ByteString]

  def allUploadedFiles(envelopeId: EnvelopeId)(implicit F: Monad[F], hc: HeaderCarrier): F[List[UploadedFile]] =
    for {
      env  <- get(envelopeId)
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

class EnvelopeService(envelopeRepo: Repo[EnvelopeData], objectStoreConnector: ObjectStoreConnector)(implicit
  ec: ExecutionContext
) extends EnvelopeAlgebra[Future] {
  private val logger = LoggerFactory.getLogger(getClass)

  override def save(envelope: EnvelopeData): Future[Unit] =
    envelopeRepo.upsert(envelope).toFuture >>
      Future.successful(logger.info(s"EnvelopeAlgebra.save(${envelope._id.value}) - upserting $envelope)"))

  override def get(envelopeId: EnvelopeId): Future[EnvelopeData] =
    envelopeRepo.get(envelopeId.value)

  override def getFileBytes(envelopeId: EnvelopeId, fileName: String)(implicit hc: HeaderCarrier): Future[ByteString] =
    objectStoreConnector.getFileBytes(envelopeId, fileName)
}
