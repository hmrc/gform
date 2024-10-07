/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.it

import uk.gov.hmrc.gform.MongoComponentSupport
import uk.gov.hmrc.gform.formmetadata.FormMetadata
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateRaw }
import uk.gov.hmrc.mongo.CurrentTimestampSupport
import uk.gov.hmrc.mongo.cache.CacheIdType.SimpleCacheId
import uk.gov.hmrc.mongo.cache.MongoCacheRepository
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait MongoDBSupport extends MongoComponentSupport {

  val mongoSettings: Map[String, String] = Map(
    "mongodb.uri" -> mongoDBURI
  )
  val formTemplateRepo: PlayMongoRepository[FormTemplate] =
    new PlayMongoRepository[FormTemplate](
      mongoComponent = mongoComponent,
      collectionName = "formTemplate",
      domainFormat = FormTemplate.format,
      indexes = Seq.empty
    )

  val formTemplateRawRepo: PlayMongoRepository[FormTemplateRaw] =
    new PlayMongoRepository[FormTemplateRaw](
      mongoComponent = mongoComponent,
      collectionName = "formTemplateRaw",
      domainFormat = FormTemplateRaw.format,
      indexes = Seq.empty
    )

  val formMetadataRepo: PlayMongoRepository[FormMetadata] =
    new PlayMongoRepository[FormMetadata](
      mongoComponent = mongoComponent,
      collectionName = "formMetadata",
      domainFormat = FormMetadata.format,
      indexes = Seq.empty
    )

  val formCacheRepository: MongoCacheRepository[String] =
    new MongoCacheRepository[String](
      mongoComponent,
      "forms",
      true,
      1.days,
      new CurrentTimestampSupport(),
      SimpleCacheId
    ) {
      override def ensureIndexes(): Future[Seq[String]] = Future.successful(Seq())
    }
}
