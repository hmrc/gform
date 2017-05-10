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

import cats.instances.future._
import play.api.libs.json._
import reactivemongo.api.{ Cursor, DefaultDB }
import reactivemongo.api.commands.WriteConcern
import reactivemongo.bson.BSONObjectID
import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.core.{ Opt, ServiceResponse, fromFutureOptA }
import uk.gov.hmrc.gform.models.{ DbOperationResult, FieldId, Form, FormId }
import uk.gov.hmrc.mongo.ReactiveRepository

import scala.concurrent.Future

class FormRepository(implicit mongo: () => DefaultDB)
    extends ReactiveRepository[Form, BSONObjectID]("forms", mongo, implicitly[Format[Form]]) {

  def findOne(
    selector: JsObject,
    projection: JsObject
  )(
    implicit
    ex: ExecutionContext
  ): Future[Option[Form]] = {
    collection.find(selector = selector, projection = projection).one[Form]
  }

  def find(
    selector: JsObject,
    projection: JsObject
  )(
    implicit
    ex: ExecutionContext
  ): Future[List[Form]] = {
    collection.find(selector = selector, projection = projection).cursor[Form]().collect[List]()
  }

  def insert(
    selector: JsObject,
    form: Form
  )(
    implicit
    ex: ExecutionContext
  ): Future[Opt[DbOperationResult]] = {
    val res = collection.update(selector = selector, update = form, writeConcern = WriteConcern.Default, upsert = true, multi = false)

    checkUpdateResult(res)
  }

  def update(
    selector: JsObject,
    form: Form
  )(
    implicit
    ex: ExecutionContext
  ): ServiceResponse[DbOperationResult] = {
    val formFieldIds: Seq[FieldId] = form.formData.fields.map(_.id)
    val dropExisting = Json.obj("$pull" -> Json.obj("fields" -> Json.obj("id" -> Json.obj("$in" -> formFieldIds))))
    val updateFields = Json.obj("$push" -> Json.obj("fields" -> Json.obj("$each" -> form.formData.fields)))

    def runCommand(update: JsObject) = {
      fromFutureOptA(checkUpdateResult(collection.update(selector = selector, update = update, writeConcern = WriteConcern.Default, upsert = true, multi = false)))
    }

    for {
      _ <- runCommand(dropExisting)
      r <- runCommand(updateFields)
    } yield r
  }
}
