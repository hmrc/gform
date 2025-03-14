/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.retrieval

import uk.gov.hmrc.crypto.{ Decrypter, Encrypter }
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.mongo.CurrentTimestampSupport
import uk.gov.hmrc.mongo.cache.CacheIdType.SimpleCacheId
import uk.gov.hmrc.mongo.cache.MongoCacheRepository

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ ExecutionContext, Future }

class AuthRetrievalModule(
  mongoModule: MongoModule,
  configModule: ConfigModule,
  jsonCrypto: Encrypter with Decrypter
)(implicit
  ex: ExecutionContext
) {
  private val mongoCacheRepository: MongoCacheRepository[String] = new MongoCacheRepository[String](
    mongoModule.mongoComponent,
    "formAuthRetrievals",
    true,
    configModule.appConfig.formExpiryDays.days,
    new CurrentTimestampSupport(),
    SimpleCacheId
  )

  private val retrievalRepository: RetrievalPersistenceAlgebra[Future] =
    new AuthRetrievalCache(mongoCacheRepository, jsonCrypto)

  private val authRetrievalService: AuthRetrievalService = new AuthRetrievalService(retrievalRepository)

  val authRetrievalController: AuthRetrievalController =
    new AuthRetrievalController(authRetrievalService, configModule.controllerComponents)
}
