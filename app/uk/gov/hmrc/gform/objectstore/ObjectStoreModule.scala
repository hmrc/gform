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

import akka.actor.ActorSystem
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.objectstore.client.config.ObjectStoreClientConfig
import uk.gov.hmrc.objectstore.client.play.PlayObjectStoreClientEither

import scala.concurrent.ExecutionContext

class ObjectStoreModule(
  configModule: ConfigModule,
  objectStoreClient: PlayObjectStoreClientEither,
  objectStoreClientConfig: ObjectStoreClientConfig
)(implicit ex: ExecutionContext, system: ActorSystem) {

  val objectStoreController: ObjectStoreController =
    new ObjectStoreController(configModule.controllerComponents, objectStoreClient, objectStoreClientConfig)

}
