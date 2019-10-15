/*
 * Copyright 2019 HM Revenue & Customs
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
import cats.syntax.either._
import play.api.libs.json._
import reactivemongo.api.{ Cursor, DefaultDB, WriteConcern }
import reactivemongo.bson.BSONObjectID

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.mongo.ReactiveRepository
import uk.gov.hmrc.play.http.logging.Mdc.preservingMdc

class Repo[T: OWrites: Manifest](name: String, mongo: () => DefaultDB, idLens: T => String)(
  implicit formatT: Format[T],
  ec: ExecutionContext)
    extends ReactiveRepository[T, BSONObjectID](name, mongo, formatT) {
  underlying =>

  import reactivemongo.play.json.ImplicitBSONHandlers._

  def find(id: String): Future[Option[T]] = preservingMdc {
    underlying.collection
      .find[JsObject, T](idSelector(id), None)
      .one[T]
  }

  def get(id: String): Future[T] = preservingMdc {
    find(id).map(_.getOrElse(throw new NoSuchElementException(s"$name for given id: '$id' not found")))
  }

  def search(selector: JsObject): Future[List[T]] = preservingMdc {
    //TODO: don't abuse it to much. If querying for a large underlyingReactiveRepository.collection.consider returning stream instead of packing everything into the list
    underlying.collection
      .find[JsObject, T](selector = selector, None)
      .updateOptions(_.batchSize(Integer.MAX_VALUE))
      .cursor[T]()
      .collect[List](Integer.MAX_VALUE, Cursor.FailOnError())
  }

  def search(selector: JsObject, orderBy: JsObject): Future[List[T]] = preservingMdc {
    //TODO: don't abuse it to much. If querying for a large underlyingReactiveRepository.collection.consider returning stream instead of packing everything into the list
    underlying.collection
      .find[JsObject, T](selector = selector, None)
      .sort(orderBy)
      .updateOptions(_.batchSize(Integer.MAX_VALUE))
      .cursor[T]()
      .collect[List](Integer.MAX_VALUE, Cursor.FailOnError())
  }

  def projection[P: Format](projection: JsObject): Future[List[P]] = preservingMdc {
    underlying.collection
      .find[JsObject, JsObject](selector = Json.obj(), Some(projection))
      .cursor[P]()
      .collect[List](Integer.MAX_VALUE, Cursor.FailOnError())
  }

  def upsert(t: T): FOpt[Unit] = EitherT {
    preservingMdc {
      underlying.collection
        .update(idSelector(t), update = t, writeConcern = WriteConcern.Default, upsert = true, multi = false)
        .asEither
    }
  }

  def delete(id: String): FOpt[Unit] = EitherT {
    preservingMdc {
      underlying.collection.delete().one(idSelector(id)).asEither
    }
  }

  private def idSelector(id: String): JsObject = Json.obj("_id" -> id)
  private def idSelector(t: T): JsObject = idSelector(idLens(t))

  implicit class FutureWriteResultOps[R](t: Future[R]) {
    def asEither: Future[Either[UnexpectedState, Unit]] =
      t.map { _ =>
        ().asRight[UnexpectedState]
      } recover {
        case lastError => UnexpectedState(lastError.getMessage).asLeft[Unit]
      }
  }
}
