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

import play.api.libs.json.Format
import uk.gov.hmrc.crypto.{ Decrypter, Encrypter }
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.mongo.cache.MongoCacheRepository
import uk.gov.hmrc.mongo.cache.DataKey

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import play.api.libs.json._

class SnapshotMongoCache(
  mongoCacheRepository: MongoCacheRepository[String],
  jsonCrypto: Encrypter with Decrypter,
  timeProvider: TimeProvider
)(implicit ec: ExecutionContext) {

  private val snapshotDataKey: DataKey[SnapshotItem] = DataKey("snapshot")
  implicit val formats: Format[SnapshotItem] = SnapshotItem.format(jsonCrypto)
  def put(id: SnapshotId, snapshotItem: SnapshotItem): Future[Unit] =
    mongoCacheRepository
      .put(id.value)(snapshotDataKey, snapshotItem)
      .map(_ => ())

  def find(snapshotId: SnapshotId): Future[Option[SnapshotItem]] = mongoCacheRepository
    .get[SnapshotItem](snapshotId.value)(snapshotDataKey)

  def findAll(): Future[List[SnapshotItem]] =
    mongoCacheRepository.collection
      .find()
      .toFuture()
      .map(_.toList)
      .map { cacheItemList =>
        cacheItemList.flatMap { cacheItem =>
          cacheItem.data.as[JsObject].value("snapshot").validate[SnapshotItem] match {
            case JsSuccess(snapshotItem, _) => Some(snapshotItem)
            case JsError(errors)            => throw new Exception(s"Error deserializing SnapshotItem: $errors")
          }
        }
      }

  def upsert(snapshotItem: SnapshotItem): Future[Unit] =
    mongoCacheRepository
      .put(snapshotItem.snapshotId.value)(snapshotDataKey, snapshotItem)
      .map(_ => ())
}
