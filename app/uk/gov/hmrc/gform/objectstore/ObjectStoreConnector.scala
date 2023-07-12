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
import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import akka.util.ByteString
import uk.gov.hmrc.gform.models.helpers.ObjectStoreHelper._
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.objectstore.client
import uk.gov.hmrc.objectstore.client.config.ObjectStoreClientConfig
import uk.gov.hmrc.objectstore.client.play.Implicits._
import uk.gov.hmrc.objectstore.client.play.PlayObjectStoreClient
import uk.gov.hmrc.objectstore.client.{ ObjectSummaryWithMd5, Path }

import scala.concurrent.{ ExecutionContext, Future }

class ObjectStoreConnector(
  objectStoreClient: PlayObjectStoreClient,
  objectStoreClientConfig: ObjectStoreClientConfig,
  zipDirectory: String
)(implicit ex: ExecutionContext, actorSystem: ActorSystem) {

  private val zipExtension = ".zip"

  private def directory(path: String, folderName: String): Path.Directory =
    Path.Directory(s"${path}envelopes/$folderName")

  def uploadFile(
    envelopeId: EnvelopeId,
    fileName: String,
    content: ByteString,
    contentType: Option[String],
    path: String
  )(implicit
    hc: HeaderCarrier
  ): Future[ObjectSummaryWithMd5] =
    objectStoreClient
      .putObject(
        path = directory(path, envelopeId.value).file(fileName),
        content = toSource(content),
        contentType = contentType
      )

  def getFileBytes(envelopeId: EnvelopeId, fileName: String, path: String)(implicit
    hc: HeaderCarrier
  ): Future[ByteString] =
    objectStoreClient
      .getObject[Source[ByteString, NotUsed]](
        path = directory(path, envelopeId.value).file(fileName)
      )
      .flatMap {
        case Some(o) =>
          for {
            content <- o.content.asString
            res     <- Future.successful(ByteString(content.getBytes()))
          } yield res
        case _ =>
          Future.failed(new RuntimeException(s"File $fileName not found in path: ${directory(path, envelopeId.value)}"))
      }

  def deleteFile(envelopeId: EnvelopeId, fileName: String, path: String)(implicit
    hc: HeaderCarrier
  ): Future[Unit] =
    objectStoreClient.deleteObject(
      path = directory(path, envelopeId.value).file(fileName)
    )

  def zipFiles(envelopeId: EnvelopeId, path: String)(implicit
    hc: HeaderCarrier
  ): Future[ObjectSummaryWithMd5] =
    objectStoreClient.zip(
      from = directory(path, envelopeId.value),
      to = Path.Directory(s"$path$zipDirectory").file(s"${envelopeId.value}$zipExtension")
    )

  def deleteZipFile(envelopeId: EnvelopeId, path: String)(implicit hc: HeaderCarrier): Future[Unit] =
    objectStoreClient.deleteObject(
      path = Path.Directory(s"$path$zipDirectory").file(s"${envelopeId.value}$zipExtension")
    )

  def getZipFile(envelopeId: EnvelopeId, path: String)(implicit
    hc: HeaderCarrier
  ): Future[Option[client.Object[Source[ByteString, NotUsed]]]] =
    objectStoreClient
      .getObject(path = Path.Directory(s"$path$zipDirectory").file(s"${envelopeId.value}$zipExtension"))

}
