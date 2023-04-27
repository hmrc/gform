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

package uk.gov.hmrc.gform.shutter

import org.mongodb.scala.bson.Document
import org.mongodb.scala.model.{ FindOneAndReplaceOptions }
import org.mongodb.scala.result.DeleteResult

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.sharedmodel.ShutterMessageId
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository

class ShutterRepository(mongoModule: MongoModule)(implicit ec: ExecutionContext)
    extends PlayMongoRepository[Shutter](
      collectionName = "shutter",
      mongoComponent = mongoModule.mongoComponent,
      domainFormat = Shutter.format,
      indexes = Seq()
    ) {

  def findAll(): Future[List[Shutter]] =
    collection
      .find()
      .toFuture()
      .map(_.toList)

  def find(shutterMessageId: ShutterMessageId): Future[Option[Shutter]] =
    collection
      .find(Document("_id" -> shutterMessageId.value))
      .headOption()

  def upsert(shutter: Shutter): Future[Shutter] =
    collection
      .findOneAndReplace(
        Document("_id" -> shutter._id.value),
        shutter,
        FindOneAndReplaceOptions().upsert(true)
      )
      .toFuture()

  def delete(shutterMessageId: ShutterMessageId): Future[DeleteResult] =
    this.collection
      .deleteOne(Document("_id" -> shutterMessageId.value))
      .toFuture()
}
