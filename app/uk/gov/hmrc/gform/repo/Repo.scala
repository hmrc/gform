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

package uk.gov.hmrc.gform.repo

import cats.data.EitherT
import cats.implicits._
import play.api.libs.json._
import reactivemongo.api.DefaultDB
import reactivemongo.api.commands.{ WriteConcern, WriteResult }
import reactivemongo.bson.BSONObjectID
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.mongo.ReactiveRepository

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Repo[T: OWrites: Manifest](name: String, mongo: () => DefaultDB, idLens: T => String)(implicit formatT: Format[T])
    extends ReactiveRepository[T, BSONObjectID](name, mongo, formatT) {
  underlying =>

  def find(id: String): Future[Option[T]] = underlying
    .collection
    .find(idSelector(id), npProjection)
    .one[T]

  def get(id: String): Future[T] = find(id).map(_.getOrElse(throw new NoSuchElementException(s"$name for given id: '$id' not found")))

  def search(selector: JsObject): Future[List[T]] = {
    //TODO: don't abuse it to much. If querying for a large underlyingReactiveRepository.collection.consider returning stream instead of packing everything into the list
    underlying.collection.find(selector = selector, npProjection).cursor[T]().collect[List]()
  }

  def upsert(t: T): FOpt[Unit] = EitherT {
    underlying
      .collection
      .update(idSelector(t), update = t, writeConcern = WriteConcern.Default, upsert = true, multi = false)
      .map(_.asEither)
  }

  def delete(id: String): FOpt[Unit] = EitherT {
    underlying
      .collection
      .remove(idSelector(id))
      .map(_.asEither)
  }

  private def idSelector(id: String): JsObject = Json.obj("_id" -> id)
  private def idSelector(t: T): JsObject = idSelector(idLens(t))
  private lazy val npProjection = Json.obj()

  implicit class WriteResultOps(w: WriteResult) {
    def asEither: Either[UnexpectedState, Unit] = if (w.ok) ().asRight else UnexpectedState(w.message).asLeft
  }
}

