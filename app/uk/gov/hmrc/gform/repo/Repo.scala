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

package uk.gov.hmrc.gform.repo

import cats.data.EitherT
import cats.instances.long._
import cats.syntax.either._
import cats.syntax.eq._
import com.mongodb.ReadPreference
import com.mongodb.client.result.UpdateResult
import org.mongodb.scala.Document
import org.mongodb.scala.bson.BsonValue
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.{ Aggregates, Field }
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.{ Filters, FindOneAndReplaceOptions, IndexModel, ReplaceOneModel, ReplaceOptions }
import play.api.libs.json._
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository
import org.mongodb.scala.model.Projections._

import scala.concurrent.{ ExecutionContext, Future }

class Repo[T: OWrites: Manifest](
  name: String,
  mongoComponent: MongoComponent,
  idLens: T => String,
  indexes: Seq[IndexModel] = Seq.empty,
  replaceIndexes: Boolean = false
)(implicit
  formatT: Format[T],
  ec: ExecutionContext
) extends PlayMongoRepository[T](mongoComponent, name, formatT, indexes, None, replaceIndexes) {
  underlying =>

  def findDocumentAsJson(id: String): Future[Option[JsValue]] =
    underlying.collection
      .find[Document](equal("_id", id))
      .first()
      .toFutureOption()
      .map(_.map(json => Json.parse(json.toJson())))

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

  def page(
    selector: Bson,
    orderBy: Bson,
    skip: Int,
    limit: Int
  ): Future[List[T]] =
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

  def countAll(): Future[Long] =
    underlying.collection
      .countDocuments()
      .toFuture()

  def aggregate(pipeline: Seq[Bson]): Future[Seq[BsonValue]] =
    underlying.collection
      .aggregate[BsonValue](pipeline)
      .toFuture()

  def projection(includes: String*): Future[List[JsValue]] =
    underlying.collection
      .find[Document]()
      .projection(include(includes: _*))
      .toFuture()
      .map(_.toList.map(d => Json.parse(d.toJson())))

  // findOneAndReplace fails if json to be replaced by t is not compatible with type T
  def upsert(t: T): FOpt[Unit] = EitherT {
    underlying.collection
      .findOneAndReplace(idSelector(t), t, FindOneAndReplaceOptions().upsert(true))
      .toFuture()
      .asEither
  }

  // replaceOne do not fails if json to be replaced by t is not compatible with type T
  def replace(t: T): FOpt[Unit] =
    EitherT {
      underlying.collection
        .replaceOne(idSelector(t), t, ReplaceOptions().upsert(true))
        .toFuture()
        .asEither
    }

  def upsertBulk(ts: Seq[T]): FOpt[Unit] = EitherT {
    underlying.collection
      .bulkWrite(
        ts.map(t =>
          new ReplaceOneModel(
            idSelector(t),
            t,
            ReplaceOptions().upsert(true)
          )
        )
      )
      .toFuture()
      .asEither
  }

  def delete(id: String): FOpt[DeleteResult] = EitherT {
    underlying.collection
      .deleteOne(Filters.equal("_id", id))
      .toFuture()
      .map(wr => DeleteResult(id, wr.getDeletedCount() === 1).asRight)
      .recover { case lastError =>
        UnexpectedState(lastError.getMessage).asLeft
      }
  }

  def deleteByFieldName(fieldName: String, id: String): FOpt[DeleteResult] = EitherT {
    underlying.collection
      .deleteMany(Filters.equal(fieldName, id))
      .toFuture()
      .map(wr => DeleteResult(id, wr.getDeletedCount() > 0).asRight)
      .recover { case lastError =>
        UnexpectedState(lastError.getMessage).asLeft
      }
  }

  def deleteAll(): FOpt[Unit] = EitherT {
    underlying.collection.deleteMany(Document()).toFuture().asEither
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

  def sdesMigration(): Future[UpdateResult] = {
    val filter: Bson = Filters.nor(Filters.exists("destination"))
    val update = Aggregates.set(
      Field("destination", SdesDestination.fromName(SdesDestination.Dms))
    )
    underlying.collection.updateMany(filter, update).toFuture()
  }

}
