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

import org.apache.pekko.NotUsed
import org.apache.pekko.stream.Materializer
import org.apache.pekko.stream.scaladsl.Source
import org.apache.pekko.util.ByteString
import play.api.libs.ws.WSClient
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.envelope.EnvelopeModule
import uk.gov.hmrc.gform.sdes.SdesConnector
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.envelope.EnvelopeData
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination
import uk.gov.hmrc.gform.submission.{ PdfAndXmlSummaries, Submission }
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.objectstore.client
import uk.gov.hmrc.objectstore.client.config.ObjectStoreClientConfig
import uk.gov.hmrc.objectstore.client.play.PlayObjectStoreClient
import uk.gov.hmrc.objectstore.client.{ ObjectSummaryWithMd5, Path, RetentionPeriod }

import scala.concurrent.{ ExecutionContext, Future }

class ObjectStoreModule(
  configModule: ConfigModule,
  wsClient: WSClient,
  akkaModule: AkkaModule,
  envelopeModule: EnvelopeModule,
  wSHttpModule: WSHttpModule
)(implicit ex: ExecutionContext) {

  private val baseUrl = configModule.serviceConfig.baseUrl("object-store")
  private val authorizationToken = configModule.typesafeConfig.getString("internal-auth.token")
  private val defaultRetentionPeriod = RetentionPeriod
    .parse(configModule.typesafeConfig.getString("object-store.default-retention-period"))
    .fold(m => throw new IllegalStateException(m), identity)

  private val objectStoreClientConfig = ObjectStoreClientConfig(
    baseUrl,
    configModule.appConfig.appName,
    authorizationToken,
    defaultRetentionPeriod
  )

  val objectStoreClient = new PlayObjectStoreClient(wsClient, objectStoreClientConfig)(akkaModule.materializer, ex)

  val objectStoreConnector: ObjectStoreConnector =
    new ObjectStoreConnector(objectStoreClient)(
      ex,
      akkaModule.actorSystem
    )

  private val sdesBaseUrl = configModule.serviceConfig.baseUrl("sdes")
  private val sdesBasePath = configModule.sdesConfig.basePath
  private val sdesKeyAndCredentialsApiBaseUrl = configModule.serviceConfig.baseUrl("sdes-key-and-credentials-api")
  private val sdesKeyAndCredentialsApiBasePath = configModule.sdesKeyAndCredentialsApiConfig.basePath

  val sdesConnector: SdesConnector =
    new SdesConnector(
      wSHttpModule.httpClient,
      sdesBaseUrl,
      sdesBasePath,
      sdesKeyAndCredentialsApiBaseUrl,
      sdesKeyAndCredentialsApiBasePath
    )

  val objectStoreService: ObjectStoreAlgebra[Future] =
    new ObjectStoreService(objectStoreConnector, envelopeModule.envelopeService, sdesConnector)

  val objectStoreController: ObjectStoreController =
    new ObjectStoreController(configModule.controllerComponents, objectStoreService)(ex, akkaModule.materializer)

  val foptObjectStoreService: ObjectStoreAlgebra[FOpt] = new ObjectStoreAlgebra[FOpt] {

    override def createEnvelope(formTemplateId: FormTemplateId): FOpt[EnvelopeId] =
      fromFutureA(objectStoreService.createEnvelope(formTemplateId))

    override def getFileBytes(envelopeId: EnvelopeId, fileName: String)(implicit hc: HeaderCarrier): FOpt[ByteString] =
      fromFutureA(objectStoreService.getFileBytes(envelopeId, fileName))

    override def uploadFile(
      envelopeId: EnvelopeId,
      fileId: FileId,
      fileName: String,
      content: ByteString,
      contentType: ContentType
    )(implicit hc: HeaderCarrier): FOpt[ObjectSummaryWithMd5] =
      fromFutureA(objectStoreService.uploadFile(envelopeId, fileId, fileName, content, contentType))

    override def getEnvelope(envelopeId: EnvelopeId): FOpt[EnvelopeData] =
      fromFutureA(objectStoreService.getEnvelope(envelopeId))

    override def deleteFile(envelopeId: EnvelopeId, fileId: FileId)(implicit hc: HeaderCarrier): FOpt[Unit] =
      fromFutureA(objectStoreService.deleteFile(envelopeId, fileId))

    override def deleteFiles(envelopeId: EnvelopeId, fileIds: Set[FileId])(implicit hc: HeaderCarrier): FOpt[Unit] =
      fromFutureA(objectStoreService.deleteFiles(envelopeId, fileIds))

    override def zipFiles(envelopeId: EnvelopeId, objectStorePaths: ObjectStorePaths)(implicit
      hc: HeaderCarrier
    ): FOpt[ObjectSummaryWithMd5] =
      fromFutureA(objectStoreService.zipFiles(envelopeId, objectStorePaths))

    override def deleteZipFile(envelopeId: EnvelopeId, objectStorePaths: ObjectStorePaths)(implicit
      hc: HeaderCarrier
    ): FOpt[Unit] =
      fromFutureA(objectStoreService.deleteZipFile(envelopeId, objectStorePaths))

    override def getZipFile(envelopeId: EnvelopeId, objectStorePaths: ObjectStorePaths)(implicit
      hc: HeaderCarrier,
      m: Materializer
    ): FOpt[Option[client.Object[Source[ByteString, NotUsed]]]] =
      fromFutureA(objectStoreService.getZipFile(envelopeId, objectStorePaths))

    override def uploadFileWithDir(
      path: Path.Directory,
      fileName: String,
      content: ByteString,
      contentType: ContentType
    )(implicit hc: HeaderCarrier): FOpt[ObjectSummaryWithMd5] =
      fromFutureA(objectStoreService.uploadFileWithDir(path, fileName, content, contentType))

    override def deleteFile(directory: Path.Directory, fileName: String)(implicit hc: HeaderCarrier): FOpt[Unit] =
      fromFutureA(objectStoreService.deleteFile(directory, fileName))

    override def getFile(directory: Path.Directory, fileName: String)(implicit
      hc: HeaderCarrier
    ): FOpt[Option[client.Object[Source[ByteString, NotUsed]]]] =
      fromFutureA(objectStoreService.getFile(directory, fileName))

    override def uploadFromUrl(
      from: java.net.URL,
      envelopeId: EnvelopeId,
      fileId: FileId,
      contentType: ContentType,
      fileName: String
    )(implicit
      hc: HeaderCarrier
    ): FOpt[ObjectSummaryWithMd5] =
      fromFutureA(objectStoreService.uploadFromUrl(from, envelopeId, fileId, contentType, fileName))

    override def submitEnvelope(
      submission: Submission,
      summaries: PdfAndXmlSummaries,
      hmrcDms: Destination.HmrcDms,
      formTemplateId: FormTemplateId
    )(implicit hc: HeaderCarrier): FOpt[Unit] =
      fromFutureA(objectStoreService.submitEnvelope(submission, summaries, hmrcDms, formTemplateId))

    override def zipAndEncrypt(envelopeId: EnvelopeId, objectStorePaths: ObjectStorePaths)(implicit
      hc: HeaderCarrier,
      m: Materializer
    ): FOpt[ObjectSummaryWithMd5] =
      fromFutureA(objectStoreService.zipAndEncrypt(envelopeId, objectStorePaths))
  }
}
