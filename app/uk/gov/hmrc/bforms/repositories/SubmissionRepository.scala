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

import play.api.libs.json._
import reactivemongo.api.{ Cursor, DefaultDB }
import reactivemongo.api.commands.WriteConcern
import reactivemongo.bson.BSONObjectID
import scala.concurrent.ExecutionContext
import uk.gov.hmrc.bforms.core.{ Opt, fromFutureOptA }
import uk.gov.hmrc.bforms.model.{ DbOperationResult, Submission, FormId }
import uk.gov.hmrc.mongo.ReactiveRepository

import scala.concurrent.Future

class SubmissionRepository(implicit mongo: () => DefaultDB)
    extends ReactiveRepository[Submission, BSONObjectID]("submission", mongo, implicitly[Format[Submission]]) {

  def findOne(
    selector: JsObject,
    projection: JsObject
  )(
    implicit
    ex: ExecutionContext
  ): Future[Option[Submission]] = {
    collection.find(selector = selector, projection = projection).one[Submission]
  }

  def insert(
    selector: JsObject,
    submission: Submission
  )(
    implicit
    ex: ExecutionContext
  ): Future[Opt[DbOperationResult]] = {
    val res = collection.update(selector = selector, update = submission, writeConcern = WriteConcern.Default, upsert = true, multi = false)

    checkUpdateResult(res)
  }
}
