/*
 * Copyright 2018 HM Revenue & Customs
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
import reactivemongo.api.commands.{ UpdateWriteResult, WriteConcern }
import reactivemongo.bson.BSONObjectID
import reactivemongo.play.json.ImplicitBSONHandlers._
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.mongo.ReactiveRepository

import scala.concurrent.{ ExecutionContext, Future }

class Repo[T: OWrites: Manifest](name: String, mongo: () => DefaultDB, idLens: T => String)(implicit formatT: Format[T])
    extends ReactiveRepository[T, BSONObjectID](name, mongo, formatT) {
  underlying =>

  def find(id: String)(implicit ec: ExecutionContext): Future[Option[T]] =
    underlying.collection
      .find(idSelector(id), npProjection)
      .one[T]

  def get(id: String)(implicit ec: ExecutionContext): Future[T] =
    find(id).map(_.getOrElse(throw new NoSuchElementException(s"$name for given id: '$id' not found")))

  def search(selector: JsObject)(implicit ec: ExecutionContext): Future[List[T]] =
    //TODO: don't abuse it to much. If querying for a large underlyingReactiveRepository.collection.consider returning stream instead of packing everything into the list
    underlying.collection.find(selector = selector, npProjection).cursor[T]().collect[List]()

  def upsert(t: T)(implicit ec: ExecutionContext): FOpt[Unit] = EitherT {
    underlying.collection
      .update(idSelector(t), update = t, writeConcern = WriteConcern.Default, upsert = true, multi = false)
      .asEither
  }

  def delete(id: String)(implicit ec: ExecutionContext): FOpt[Unit] = EitherT {
    underlying.collection.remove(idSelector(id)).asEither
  }

  private def idSelector(id: String)(implicit ec: ExecutionContext): JsObject = Json.obj("_id" -> id)
  private def idSelector(t: T)(implicit ec: ExecutionContext): JsObject = idSelector(idLens(t))
  private lazy val npProjection = Json.obj()

  implicit class FutureWriteResultOps[T](t: Future[T])(implicit ec: ExecutionContext) {
    def asEither: Future[Either[UnexpectedState, Unit]] =
      t.map {
        case _ => ().asRight
      } recover {
        case lastError =>
          UnexpectedState(lastError.getMessage).asLeft
      }
  }
}
