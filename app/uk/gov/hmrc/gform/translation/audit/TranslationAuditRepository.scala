/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.translation.audit

import cats.data.EitherT
import cats.syntax.all._
import java.util.concurrent.TimeUnit
import org.bson.conversions.Bson
import org.mongodb.scala.bson.{ BsonValue, Document }
import org.mongodb.scala.model.Sorts.descending
import org.mongodb.scala.model.{ Aggregates, IndexModel, IndexOptions, Indexes }
import org.mongodb.scala.model.Filters.equal
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.mongo.play.json.{ Codecs, PlayMongoRepository }

class TranslationAuditRepository(mongoModule: MongoModule, appConfig: AppConfig)(implicit ec: ExecutionContext)
    extends PlayMongoRepository[TranslationAudit](
      collectionName = "translationAudit",
      mongoComponent = mongoModule.mongoComponent,
      domainFormat = TranslationAudit.format,
      indexes = Seq(
        IndexModel(
          Indexes.ascending("createdAt"),
          IndexOptions()
            .background(false)
            .name("createdAtIndex")
            .expireAfter(appConfig.`translation-audit-ttl`.toMillis, TimeUnit.MILLISECONDS)
        )
      )
    ) {

  def all(): Future[Seq[TranslationAuditOverview]] = {
    val sort = Aggregates.sort(descending("createdAt"))

    val pipeline = List(sort)

    collection
      .aggregate[BsonValue](pipeline)
      .toFuture()
      .map(_.map(Codecs.fromBson[TranslationAuditOverview]))
  }

  def find(historyId: TranslationAuditId): Future[Option[TranslationAudit]] =
    collection
      .find(Document("_id" -> historyId.toObjectId))
      .headOption()

  def upsert(formTemplateHistory: TranslationAudit): FOpt[Unit] =
    EitherT {
      collection
        .insertOne(formTemplateHistory)
        .toFuture()
        .asEither
    }

  def latestAudit(formTemplateId: FormTemplateId): Future[Option[TranslationAuditOverview]] = {

    val filter: Bson = Aggregates.filter(equal("formTemplateId", formTemplateId.value))

    val sort = Aggregates.sort(descending("createdAt"))

    val pipeline = List(filter, sort)

    collection
      .aggregate[BsonValue](pipeline)
      .headOption()
      .map(_.map(Codecs.fromBson[TranslationAuditOverview]))
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
