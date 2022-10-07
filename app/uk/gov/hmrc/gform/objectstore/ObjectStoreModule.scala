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

import play.api.libs.ws.WSClient
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.objectstore.client.RetentionPeriod
import uk.gov.hmrc.objectstore.client.config.ObjectStoreClientConfig
import uk.gov.hmrc.objectstore.client.play.PlayObjectStoreClient

import scala.concurrent.ExecutionContext

class ObjectStoreModule(
  configModule: ConfigModule,
  wsClient: WSClient,
  akkaModule: AkkaModule
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
    new ObjectStoreConnector(objectStoreClient, objectStoreClientConfig)(
      ex,
      akkaModule.actorSystem
    )

}
