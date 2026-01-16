/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.upscan

import org.mongodb.scala.model.{ Filters, FindOneAndReplaceOptions, IndexModel, IndexOptions, Indexes, ReturnDocument }
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository

import java.util.concurrent.TimeUnit
import scala.concurrent.{ ExecutionContext, Future }

class UpscanDuplicateCheckRepository(
  appConfig: AppConfig,
  mongoComponent: MongoComponent
)(implicit ec: ExecutionContext)
    extends PlayMongoRepository[UpscanDuplicateCheck](
      collectionName = "upscanDuplicateCheck",
      mongoComponent = mongoComponent,
      domainFormat = UpscanDuplicateCheck.format,
      indexes = Seq(
        IndexModel(
          Indexes.ascending("receivedAt"),
          IndexOptions()
            .background(false)
            .name("receivedAtIdx")
            .expireAfter(appConfig.`upscan-duplicate-check-ttl`.toMillis, TimeUnit.MILLISECONDS)
        )
      )
    ) {

  def find(reference: UpscanReference): Future[Option[UpscanDuplicateCheck]] =
    collection
      .find(Filters.equal("_id", reference.value))
      .headOption()

  def upsert(confirmation: UpscanDuplicateCheck): Future[UpscanDuplicateCheck] =
    collection
      .findOneAndReplace(
        Filters.equal("_id", confirmation._id.value),
        confirmation,
        FindOneAndReplaceOptions().upsert(true).returnDocument(ReturnDocument.AFTER)
      )
      .toFuture()

}
