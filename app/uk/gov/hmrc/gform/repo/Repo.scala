/*
 * Copyright 2021 HM Revenue & Customs
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
import com.mongodb.ReadPreference
import org.bson.Document
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.{ Filters, FindOneAndReplaceOptions, IndexModel, InsertManyOptions }
import play.api.libs.json._
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository
import org.mongodb.scala.model.Projections._
import scala.concurrent.{ ExecutionContext, Future }

class Repo[T: OWrites: Manifest](
  name: String,
  mongoComponent: MongoComponent,
  idLens: T => String,
  indexes: Seq[IndexModel] = Seq.empty
)(implicit
  formatT: Format[T],
  ec: ExecutionContext
) extends PlayMongoRepository[T](mongoComponent, name, formatT, indexes) {
  underlying =>

  def findDocumentAsJson(id: String): Future[Option[JsValue]] =
    underlying.collection
      .find[Document](equal("_id", id))
      .first()
      .toFutureOption()
      .map(_.map(json => Json.parse(json.toJson)))

  def getDocumentAsJson(id: String): Future[JsValue] =
    findDocumentAsJson(id).map(_.getOrElse(throw new NoSuchElementException(s"$name for given id: '$id' not found")))

  def find(id: String): Future[Option[T]] =
    underlying.collection
      .find(equal("_id", id))
      .first()
      .toFutureOption()

  def findAll(): Future[List[T]] = underlying.collection
    .withReadPreference(ReadPreference.secondaryPreferred)
    .find()
    .toFuture()
    .map(_.toList)

  def get(id: String): Future[T] =
    find(id).map(_.getOrElse(throw new NoSuchElementException(s"$name for given id: '$id' not found")))

  def search(selector: Bson): Future[List[T]] =
    //TODO: don't abuse it to much. If querying for a large underlyingReactiveRepository.collection.consider returning stream instead of packing everything into the list
    underlying.collection
      .find(selector)
      .batchSize(Integer.MAX_VALUE)
      .toFuture()
      .map(_.toList)

  def search(selector: Bson, orderBy: Bson): Future[List[T]] =
    //TODO: don't abuse it to much. If querying for a large underlyingReactiveRepository.collection.consider returning stream instead of packing everything into the list
    underlying.collection
      .find(selector)
      .batchSize(Integer.MAX_VALUE)
      .sort(orderBy)
      .toFuture()
      .map(_.toList)

  def page(selector: Bson, orderBy: Bson, skip: Int, limit: Int): Future[List[T]] =
    underlying.collection
      .find(selector)
      .sort(orderBy)
      .skip(skip)
      .limit(limit)
      .toFuture()
      .map(_.toList)

  def count(selector: Bson): Future[Long] =
    underlying.collection
      .countDocuments(selector)
      .toFuture()

  def projection(includes: String*): Future[List[JsValue]] =
    underlying.collection
      .find[Document]()
      .projection(include(includes: _*))
      .toFuture()
      .map(_.toList.map(d => Json.parse(d.toJson())))

  def upsert(t: T): FOpt[Unit] = EitherT {
    underlying.collection
      .findOneAndReplace(idSelector(t), t, FindOneAndReplaceOptions().upsert(true))
      .toFuture()
      .asEither
  }

  def upsertBulk(t: Seq[T]): Future[Either[UnexpectedState, Unit]] =
    underlying.collection.insertMany(t, InsertManyOptions().ordered(false)).toFuture().asEither

  def delete(id: String): FOpt[Unit] = EitherT {
    underlying.collection
      .deleteOne(Filters.equal("_id", id))
      .toFuture()
      .asEither
  }

  private def idSelector(item: T): Bson = Filters.equal("_id", idLens(item))

  implicit class FutureWriteResultOps[R](t: Future[R]) {
    def asEither: Future[Either[UnexpectedState, Unit]] =
      t.map { _ =>
        ().asRight[UnexpectedState]
      } recover { case lastError =>
        UnexpectedState(lastError.getMessage).asLeft[Unit]
      }
  }
}
