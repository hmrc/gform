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

package uk.gov.hmrc.bforms.repositories

import play.api.Logger
import play.api.libs.json._
import reactivemongo.api.DB
import reactivemongo.api.commands.WriteConcern
import reactivemongo.bson.BSONObjectID
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.bforms.core.Opt
import uk.gov.hmrc.bforms.exceptions.InvalidState
import uk.gov.hmrc.bforms.models.{ FormTemplate, DbOperationResult, UpdateSuccess }
import uk.gov.hmrc.mongo.ReactiveRepository

class FormTemplateRepository(implicit mongo: () => DB)
    extends ReactiveRepository[FormTemplate, BSONObjectID]("formTemplate", mongo, implicitly[Format[FormTemplate]]) {

  def update(
    selector: JsObject,
    update: FormTemplate
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
  ): Future[Option[FormTemplate]] = {
    collection.find(selector = selector, projection = projection).one[FormTemplate]
  }
}
