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

import cats.syntax.functor._
import org.apache.pekko.NotUsed
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.scaladsl.Source
import org.apache.pekko.util.ByteString
import uk.gov.hmrc.gform.models.helpers.ObjectStoreHelper._
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.objectstore.client
import uk.gov.hmrc.objectstore.client.play.Implicits._
import uk.gov.hmrc.objectstore.client.play.PlayObjectStoreClient
import uk.gov.hmrc.objectstore.client.{ ObjectSummaryWithMd5, Path }

import scala.concurrent.{ ExecutionContext, Future }

class ObjectStoreConnector(
  objectStoreClient: PlayObjectStoreClient
)(implicit ex: ExecutionContext, actorSystem: ActorSystem) {

  private val zipExtension = ".zip"

  private def directory(folderName: String): Path.Directory =
    Path.Directory(s"envelopes/$folderName")

  def uploadFile(
    envelopeId: EnvelopeId,
    fileName: String,
    content: ByteString,
    contentType: Option[String]
  )(implicit
    hc: HeaderCarrier
  ): Future[ObjectSummaryWithMd5] =
    objectStoreClient
      .putObject(
        path = directory(envelopeId.value).file(fileName),
        content = toSource(content),
        contentType = contentType
      )

  def uploadFile(directory: Path.Directory, fileName: String, content: ByteString, contentType: Option[String])(implicit
    hc: HeaderCarrier
  ): Future[ObjectSummaryWithMd5] =
    objectStoreClient
      .putObject(
        path = directory.file(fileName),
        content = toSource(content),
        contentType = contentType
      )

  def getFile(directory: Path.Directory, fileName: String)(implicit
    hc: HeaderCarrier
  ): Future[Option[client.Object[Source[ByteString, NotUsed]]]] =
    objectStoreClient.getObject(directory.file(fileName))

  def getFileBytes(envelopeId: EnvelopeId, fileName: String)(implicit
    hc: HeaderCarrier
  ): Future[ByteString] =
    objectStoreClient
      .getObject[Source[ByteString, NotUsed]](
        path = directory(envelopeId.value).file(fileName)
      )
      .flatMap {
        case Some(o) =>
          for {
            content <- o.content.asString
            res     <- Future.successful(ByteString(content.getBytes()))
          } yield res
        case _ =>
          Future.failed(new RuntimeException(s"File $fileName not found in path: ${directory(envelopeId.value)}"))
      }

  def deleteFile(envelopeId: EnvelopeId, fileName: String)(implicit
    hc: HeaderCarrier
  ): Future[Unit] =
    objectStoreClient.deleteObject(
      path = directory(envelopeId.value).file(fileName)
    )

  def deleteFiles(envelopeId: EnvelopeId, fileNames: List[String])(implicit
    hc: HeaderCarrier
  ): Future[Unit] =
    Future
      .traverse(fileNames)(fileName =>
        objectStoreClient.deleteObject(
          path = directory(envelopeId.value).file(fileName)
        )
      )
      .void

  def deleteFile(directory: Path.Directory, fileName: String)(implicit
    hc: HeaderCarrier
  ): Future[Unit] =
    objectStoreClient.deleteObject(
      path = directory.file(fileName)
    )

  def zipFiles(envelopeId: EnvelopeId, objectStorePaths: ObjectStorePaths)(implicit
    hc: HeaderCarrier
  ): Future[ObjectSummaryWithMd5] =
    objectStoreClient.zip(
      from = objectStorePaths.permanent,
      to = objectStorePaths.ephemeral.file(s"${objectStorePaths.zipFilePrefix}${envelopeId.value}$zipExtension")
    )

  def deleteZipFile(envelopeId: EnvelopeId, objectStorePaths: ObjectStorePaths)(implicit
    hc: HeaderCarrier
  ): Future[Unit] =
    objectStoreClient.deleteObject(
      path = objectStorePaths.ephemeral.file(s"${objectStorePaths.zipFilePrefix}${envelopeId.value}$zipExtension")
    )

  def getZipFile(envelopeId: EnvelopeId, objectStorePaths: ObjectStorePaths)(implicit
    hc: HeaderCarrier
  ): Future[Option[client.Object[Source[ByteString, NotUsed]]]] =
    objectStoreClient
      .getObject(path =
        objectStorePaths.ephemeral.file(s"${objectStorePaths.zipFilePrefix}${envelopeId.value}$zipExtension")
      )

  def uploadFromUrl(from: java.net.URL, envelopeId: EnvelopeId, fileName: String)(implicit
    hc: HeaderCarrier
  ): Future[ObjectSummaryWithMd5] =
    objectStoreClient.uploadFromUrl(from, to = directory(envelopeId.value).file(fileName))
}
