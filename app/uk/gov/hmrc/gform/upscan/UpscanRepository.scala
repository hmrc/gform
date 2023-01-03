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

package uk.gov.hmrc.gform.upscan

import org.mongodb.scala.WriteConcern
import org.mongodb.scala.model.{ Filters, FindOneAndReplaceOptions, IndexModel, IndexOptions, Indexes, ReturnDocument }
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository
import java.util.concurrent.TimeUnit

class UpscanRepository(
  appConfig: AppConfig,
  mongoComponent: MongoComponent
)(implicit ec: ExecutionContext)
    extends PlayMongoRepository[UpscanConfirmation](
      collectionName = "upscan",
      mongoComponent = mongoComponent,
      domainFormat = UpscanConfirmation.format,
      indexes = Seq(
        IndexModel(
          Indexes.ascending("confirmedAt"),
          IndexOptions()
            .background(false)
            .name("confirmedAtIndex")
            .expireAfter(appConfig.`upscan-confirmation-ttl`.toMillis, TimeUnit.MILLISECONDS)
        )
      ),
      replaceIndexes = true
    ) {

  def find(reference: UpscanReference): Future[Option[UpscanConfirmation]] =
    collection
      .find(Filters.equal("_id", reference.value))
      .headOption()

  def upsert(confirmation: UpscanConfirmation): Future[UpscanConfirmation] =
    collection
      .findOneAndReplace(
        Filters.equal("_id", confirmation._id.value),
        confirmation,
        FindOneAndReplaceOptions().upsert(true).returnDocument(ReturnDocument.AFTER)
      )
      .toFuture()

  def delete(reference: UpscanReference): Future[Unit] =
    this.collection
      .withWriteConcern(WriteConcern.ACKNOWLEDGED)
      .deleteOne(
        filter = Filters.equal("_id", reference.value)
      )
      .toFuture()
      .map(_ => ())
}
