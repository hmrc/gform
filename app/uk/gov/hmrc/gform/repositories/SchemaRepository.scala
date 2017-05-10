/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.repositories

import play.api.Logger
import play.api.libs.json._
import reactivemongo.api.DB
import reactivemongo.bson.BSONObjectID
import reactivemongo.api.commands.WriteConcern
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.InvalidState
import uk.gov.hmrc.gform.models.{ DbOperationResult, Schema, UpdateSuccess }
import uk.gov.hmrc.mongo.ReactiveRepository

class SchemaRepository(implicit mongo: () => DB)
    extends ReactiveRepository[Schema, BSONObjectID]("schemas", mongo, implicitly[Format[Schema]]) {

  def update(
    selector: JsObject,
    update: Schema
  )(
    implicit
    ex: ExecutionContext
  ): Future[Opt[DbOperationResult]] = {
    val res = collection.update(selector = selector, update = update.value, writeConcern = WriteConcern.Default, upsert = true, multi = false)

    checkUpdateResult(res)
  }

  def findOne(
    selector: JsObject,
    projection: JsObject
  )(
    implicit
    ex: ExecutionContext
  ): Future[Option[Schema]] = {
    collection.find(selector = selector, projection = projection).one[Schema]
  }
}
