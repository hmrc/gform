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

package uk.gov.hmrc.gform.testonly

import uk.gov.hmrc.mongo.cache.MongoCacheRepository
import uk.gov.hmrc.mongo.cache.DataKey
import org.mongodb.scala.model.{ Aggregates, Filters }
import org.mongodb.scala.model.Sorts.descending

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import play.api.libs.json._

class SnapshotMongoCache(
  mongoCacheRepository: MongoCacheRepository[String]
)(implicit ec: ExecutionContext) {

  private val snapshotDataKey: DataKey[Snapshot] = DataKey("snapshot")
  def put(id: SnapshotId, snapshot: Snapshot): Future[Unit] =
    mongoCacheRepository
      .put(id.value)(snapshotDataKey, snapshot)
      .map(_ => ())

  def find(snapshotId: SnapshotId): Future[Option[Snapshot]] = mongoCacheRepository
    .get[Snapshot](snapshotId.value)(snapshotDataKey)

  def findWithFilter(snapshotFilter: SnapshotFilter): Future[List[Snapshot]] = {

    val fromFilter = snapshotFilter.from.map(from => Aggregates.filter(Filters.gte("modifiedDetails.createdAt", from)))
    val toFilter = snapshotFilter.to.map(to => Aggregates.filter(Filters.lte("modifiedDetails.createdAt", to)))
    val snapshotIdFilter =
      snapshotFilter.snapshotIdFilter.map(snapshotId => Aggregates.filter(Filters.eq("_id", snapshotId)))
    val descriptionFilter = snapshotFilter.descriptionFilter.map(description =>
      Aggregates.`match`(Filters.regex("data.snapshot.description", s".*$description.*"))
    )
    val templateIdFilter = snapshotFilter.templateIdFilter.map(templateId =>
      Aggregates.filter(Filters.eq("data.snapshot.originalTemplate._id", templateId))
    )

    val sort = Some(Aggregates.sort(descending("createdAt")))

    val limit = Some(Aggregates.limit(1000))

    val pipeline =
      List(fromFilter, toFilter, snapshotIdFilter, templateIdFilter, descriptionFilter, sort, limit).flatten

    mongoCacheRepository.collection
      .aggregate(pipeline)
      .toFuture()
      .map(_.toList)
      .map { cacheItemList =>
        cacheItemList.flatMap { cacheItem =>
          cacheItem.data.as[JsObject].value("snapshot").validate[Snapshot] match {
            case JsSuccess(snapshot, _) => Some(snapshot)
            case JsError(errors)        => throw new Exception(s"Error deserializing Snapshot: $errors")
          }
        }
      }
  }

  def upsert(snapshot: Snapshot): Future[Unit] =
    mongoCacheRepository
      .put(snapshot.snapshotId.value)(snapshotDataKey, snapshot)
      .map(_ => ())

  def delete(snapshotId: SnapshotId): Future[Unit] =
    mongoCacheRepository.deleteEntity(snapshotId.value)
}
