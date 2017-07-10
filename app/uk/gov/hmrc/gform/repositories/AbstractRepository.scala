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
import reactivemongo.api.commands.{ UpdateWriteResult, WriteConcern }
import reactivemongo.bson.BSONObjectID

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.core.{ Opt, ServiceResponse, fromFutureOptA }
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.mongo.ReactiveRepository

import scala.concurrent.Future

class AbstractRepo[T: Reads: Writes: OWrites](name: String)(implicit mongo: () => DefaultDB, formatT: Format[T], m: Manifest[T])
    extends ReactiveRepository[T, BSONObjectID](name, mongo, formatT) {

  def findOne(
    selector: JsObject,
    projection: JsObject
  )(
    implicit
    ex: ExecutionContext
  ): Future[Option[T]] = {
    collection.find(selector = selector, projection = projection).one[T]
  }

  def find(
    selector: JsObject,
    projection: JsObject
  )(
    implicit
    ex: ExecutionContext
  ): Future[List[T]] = {
    collection.find(selector = selector, projection = projection).cursor[T]().collect[List]()
  }

  def insert(
    selector: JsObject,
    obj: T
  )(
    implicit
    ex: ExecutionContext
  ): Future[Opt[DbOperationResult]] = {
    val res = collection.update(selector = selector, update = obj, writeConcern = WriteConcern.Default, upsert = true, multi = false)

    checkResult(res)

  }

  def update(
    selector: JsObject,
    update: T
  )(
    implicit
    ex: ExecutionContext
  ): ServiceResponse[DbOperationResult] = {
    val res = collection.update(selector = selector, update = update, writeConcern = WriteConcern.Default, upsert = true, multi = false)

    fromFutureOptA(checkResult(res))
  }

  def delete(selector: JsObject)(
    implicit
    ex: ExecutionContext
  ): ServiceResponse[DbOperationResult] = {
    fromFutureOptA(checkResult(collection.remove(selector)))
  }

}

class FormRepository(implicit mongo: () => DefaultDB, formatT: Format[Form]) extends AbstractRepo[Form]("forms") {

  override def update(
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
      fromFutureOptA(checkResult(collection.update(selector = selector, update = update, writeConcern = WriteConcern.Default, upsert = true, multi = false)))
    }

    for {
      _ <- runCommand(dropExisting)
      r <- runCommand(updateFields)
    } yield r
  }
}

class SchemaRepository(implicit mongo: () => DefaultDB, formatT: Format[Schema]) extends AbstractRepo[Schema]("schemas")
class FormTemplateRepository(implicit mongo: () => DefaultDB, formatT: Format[FormTemplate]) extends AbstractRepo[FormTemplate]("formTemplate")
