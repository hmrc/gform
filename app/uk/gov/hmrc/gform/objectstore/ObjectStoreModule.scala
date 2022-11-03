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
import play.api.libs.ws.WSClient
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.envelope.EnvelopeModule
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.envelope.EnvelopeData
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.objectstore.client.{ ObjectSummaryWithMd5, RetentionPeriod }
import uk.gov.hmrc.objectstore.client.config.ObjectStoreClientConfig
import uk.gov.hmrc.objectstore.client.play.PlayObjectStoreClient

import scala.concurrent.{ ExecutionContext, Future }

class ObjectStoreModule(
  configModule: ConfigModule,
  wsClient: WSClient,
  akkaModule: AkkaModule,
  envelopeModule: EnvelopeModule
)(implicit ex: ExecutionContext) {

  private val baseUrl = configModule.serviceConfig.baseUrl("object-store")
  private val authorizationToken = configModule.typesafeConfig.getString("internal-auth.token")
  private val defaultRetentionPeriod = RetentionPeriod
    .parse(configModule.typesafeConfig.getString("object-store.default-retention-period"))
    .fold(m => throw new IllegalStateException(m), identity)
  private val zipDirectory = configModule.typesafeConfig.getString("object-store.zip-directory")

  private val objectStoreClientConfig = ObjectStoreClientConfig(
    baseUrl,
    configModule.appConfig.appName,
    authorizationToken,
    defaultRetentionPeriod
  )

  val objectStoreClient = new PlayObjectStoreClient(wsClient, objectStoreClientConfig)(akkaModule.materializer, ex)

  val objectStoreConnector: ObjectStoreConnector =
    new ObjectStoreConnector(objectStoreClient, objectStoreClientConfig, zipDirectory)(
      ex,
      akkaModule.actorSystem
    )

  val objectStoreService: ObjectStoreAlgebra[Future] =
    new ObjectStoreService(objectStoreConnector, envelopeModule.envelopeService)

  val objectStoreController: ObjectStoreController =
    new ObjectStoreController(configModule.controllerComponents, objectStoreService)

  val foptObjectStoreService: ObjectStoreAlgebra[FOpt] = new ObjectStoreAlgebra[FOpt] {
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

    override def zipFiles(envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): FOpt[ObjectSummaryWithMd5] =
      fromFutureA(objectStoreService.zipFiles(envelopeId))
  }
}
