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

package uk.gov.hmrc.gform.notificationbanner

import org.mongodb.scala.bson.Document
import org.mongodb.scala.model.{ FindOneAndReplaceOptions, IndexModel, IndexOptions, Indexes }
import org.mongodb.scala.result.DeleteResult

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.sharedmodel.BannerId
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository

class NotificationBannerRepository(mongoModule: MongoModule)(implicit ec: ExecutionContext)
    extends PlayMongoRepository[NotificationBanner](
      collectionName = "notificationBanner",
      mongoComponent = mongoModule.mongoComponent,
      domainFormat = NotificationBanner.format,
      indexes = Seq(
        IndexModel(
          Indexes.ascending("isGlobal"),
          IndexOptions()
            .background(true)
            .name("isGlobal")
        )
      )
    ) {

  def findAll(): Future[List[NotificationBanner]] =
    collection
      .find()
      .toFuture()
      .map(_.toList)

  def find(bannerId: BannerId): Future[Option[NotificationBanner]] =
    collection
      .find(Document("_id" -> bannerId.value))
      .headOption()

  def findGlobal(): Future[Option[NotificationBanner]] =
    collection
      .find(Document("isGlobal" -> true))
      .headOption()

  def upsert(notificationBanner: NotificationBanner): Future[NotificationBanner] =
    collection
      .findOneAndReplace(
        Document("_id" -> notificationBanner._id.value),
        notificationBanner,
        FindOneAndReplaceOptions().upsert(true)
      )
      .toFuture()

  def delete(bannerId: BannerId): Future[DeleteResult] =
    this.collection
      .deleteOne(Document("_id" -> bannerId.value))
      .toFuture()
}
