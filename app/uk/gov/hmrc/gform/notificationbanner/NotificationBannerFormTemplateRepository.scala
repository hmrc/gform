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
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.sharedmodel.BannerId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository

import scala.concurrent.{ ExecutionContext, Future }

class NotificationBannerFormTemplateRepository(mongoModule: MongoModule)(implicit ec: ExecutionContext)
    extends PlayMongoRepository[NotificationBannerFormTemplate](
      collectionName = "notificationBannerFormTemplate",
      mongoComponent = mongoModule.mongoComponent,
      domainFormat = NotificationBannerFormTemplate.format,
      indexes = Seq(
        IndexModel(
          Indexes.ascending("bannerId"),
          IndexOptions()
            .background(true)
            .name("bannerId")
        )
      )
    ) {

  def findByBannerId(bannerId: BannerId): Future[List[NotificationBannerFormTemplate]] =
    collection
      .find(Document("bannerId" -> bannerId.value))
      .toFuture()
      .map(_.toList)

  def find(formTemplateId: FormTemplateId): Future[Option[NotificationBannerFormTemplate]] =
    collection
      .find(Document("_id" -> formTemplateId.value))
      .headOption()

  def upsert(notificationBannerFormTemplate: NotificationBannerFormTemplate): Future[NotificationBannerFormTemplate] =
    collection
      .findOneAndReplace(
        Document("_id" -> notificationBannerFormTemplate._id.value),
        notificationBannerFormTemplate,
        FindOneAndReplaceOptions().upsert(true)
      )
      .toFuture()

  def delete(formTemplateId: FormTemplateId): Future[DeleteResult] =
    this.collection
      .deleteOne(Document("_id" -> formTemplateId.value))
      .toFuture()
}
