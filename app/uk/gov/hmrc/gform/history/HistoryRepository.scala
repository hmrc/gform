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

package uk.gov.hmrc.gform.history

import cats.Eq
import cats.data.EitherT
import cats.syntax.all._
import java.time.Instant
import java.util.concurrent.TimeUnit
import org.bson.types.ObjectId
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.bson.{ BsonValue, Document }
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Projections.computed
import org.mongodb.scala.model.Sorts.descending
import org.mongodb.scala.model.{ Aggregates, Updates }
import org.mongodb.scala.model.Filters.equal
import org.mongodb.scala.model.{ IndexModel, IndexOptions, Indexes }
import play.api.libs.json.{ Json, OFormat, Reads, Writes }
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateRawId
import uk.gov.hmrc.mongo.play.json.{ Codecs, PlayMongoRepository }
import play.api.libs.json._

final case class HistoryOverviewFull(
  _id: HistoryId,
  id: FormTemplateRawId,
  createdAt: Instant,
  size: Int
)

object HistoryOverviewFull {
  implicit val format: OFormat[HistoryOverviewFull] = {
    implicit val instantReads = uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats.instantReads
    Json.format[HistoryOverviewFull] // Reads from Mongo, Writes is derived
  }
}

final case class HistoryOverview(
  _id: HistoryId,
  createdAt: Instant,
  size: Int
)

final case class HistoryId(id: String) {
  def toObjectId: ObjectId = new ObjectId(id)
}

object HistoryId {

  implicit val equal: Eq[HistoryId] = Eq.fromUniversalEquals

  implicit val bsonRead: Reads[HistoryId] =
    (__ \ "$oid").read[String].map { bsonId =>
      new HistoryId(bsonId)
    }

  implicit val bsonReadWrite: Writes[HistoryId] = new Writes[HistoryId] {
    def writes(bsonId: HistoryId): JsValue = JsString(bsonId.id)
  }

  val flatReads: Reads[HistoryId] = Reads[HistoryId] {
    case JsString(value) => JsSuccess(HistoryId(value))
    case otherwise       => JsError(s"Invalid historyId, expected JsString, got: $otherwise")
  }

}

object HistoryOverview {
  implicit val format: OFormat[HistoryOverview] = {
    implicit val instantReads = uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats.instantReads
    Json.format[HistoryOverview] // Reads from Mongo, Writes is derived
  }
}

class HistoryRepository(mongoModule: MongoModule, appConfig: AppConfig)(implicit ec: ExecutionContext)
    extends PlayMongoRepository[FormTemplateHistory](
      collectionName = "history",
      mongoComponent = mongoModule.mongoComponent,
      domainFormat = FormTemplateHistory.mongoFormat,
      indexes = Seq(
        IndexModel(
          Indexes.ascending("createdAt"),
          IndexOptions()
            .background(false)
            .name("createdAtIndex")
            .expireAfter(appConfig.`history-ttl`.toMillis, TimeUnit.MILLISECONDS)
        )
      )
    ) {

  def allFormTemplateIds(): Future[List[FormTemplateRawId]] =
    collection
      .distinct[String]("id")
      .toFuture()
      .map(_.toList.map(FormTemplateRawId(_)))

  def overviewWithFilter(historyFilter: HistoryFilter): Future[Seq[HistoryOverviewFull]] = {

    val filter: Bson = (historyFilter.from, historyFilter.to) match {
      case (Some(from), Some(to)) =>
        Aggregates.filter(
          Filters.and(
            from.gte,
            to.lte
          )
        )

      case (Some(from), None) =>
        Aggregates.filter(
          from.gte
        )

      case (None, Some(to)) =>
        Aggregates.filter(
          to.lte
        )

      case (None, None) => Aggregates.filter(Filters.empty())
    }

    val project = Aggregates.project(
      Updates.combine(
        computed("id", "$id"),
        computed("createdAt", "$createdAt"),
        computed("size", computed("$bsonSize", "$$ROOT"))
      )
    )

    val sort = Aggregates.sort(descending("createdAt"))

    val limit = Aggregates.limit(1000)

    val pipeline = List(filter, project, sort, limit)

    collection
      .aggregate[BsonValue](pipeline)
      .toFuture()
      .map(_.map(Codecs.fromBson[HistoryOverviewFull]))
  }

  def overviewFor(formTemplateRawId: FormTemplateRawId): Future[Seq[HistoryOverview]] = {
    val filter = Aggregates.filter(
      equal("id", formTemplateRawId.value)
    )

    val project = Aggregates.project(
      Updates.combine(
        computed("createdAt", "$createdAt"),
        computed("size", computed("$bsonSize", "$$ROOT"))
      )
    )

    val sort = Aggregates.sort(descending("createdAt"))

    val pipeline = List(filter, project, sort)

    collection
      .aggregate[BsonValue](pipeline)
      .toFuture()
      .map(_.map(Codecs.fromBson[HistoryOverview]))
  }

  def find(historyId: HistoryId): Future[Option[FormTemplateHistory]] =
    collection
      .find(Document("_id" -> historyId.toObjectId))
      .headOption()

  def upsert(formTemplateHistory: FormTemplateHistory): FOpt[Unit] =
    EitherT {
      collection
        .insertOne(formTemplateHistory)
        .toFuture()
        .asEither
    }

  implicit class FutureWriteResultOps[R](t: Future[R]) {
    def asEither: Future[Either[UnexpectedState, Unit]] =
      t.map { _ =>
        ().asRight[UnexpectedState]
      } recover { case lastError =>
        UnexpectedState(lastError.getMessage).asLeft[Unit]
      }
  }
}
