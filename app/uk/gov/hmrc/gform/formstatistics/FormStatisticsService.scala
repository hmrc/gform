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

package uk.gov.hmrc.gform.formstatistics

import java.time.LocalDateTime
import org.bson.conversions.Bson
import org.mongodb.scala.bson.{ BsonArray, BsonDocument, BsonValue, Document }
import org.mongodb.scala.model.Accumulators.{ first, sum }
import org.mongodb.scala.model.{ Accumulators, Aggregates, Field, Filters, Updates }
import org.mongodb.scala.model.Filters.{ and, equal, gte, lte, notEqual }
import org.mongodb.scala.model.Projections.{ computed, excludeId }
import org.mongodb.scala.model.Sorts.{ ascending, descending }
import uk.gov.hmrc.gform.formtemplate.FormTemplateService
import uk.gov.hmrc.gform.sharedmodel.form.{ AllSavedVersions, SavedFormDetail, Signed, SignedFormDetails, Submitted, VersionStats }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, NotPermitted }
import uk.gov.hmrc.mongo.cache.MongoCacheRepository
import uk.gov.hmrc.mongo.play.json.Codecs
import uk.gov.hmrc.mongo.play.json.Codecs.JsonOps

import java.time.{ LocalDate, ZoneId }
import scala.concurrent.{ ExecutionContext, Future }

trait FormStatisticsAlgebra[F[_]] {
  def getAllSavedVersions(): F[AllSavedVersions]
  def getSavedFormCount(formTemplateId: FormTemplateId): F[Seq[VersionStats]]
  def getSavedFormDetails(formTemplateId: FormTemplateId): F[Seq[SavedFormDetail]]
  def getSignedFormDetails(): F[Seq[SignedFormDetails]]
}

class FormStatisticsService(
  formsCacheRepository: MongoCacheRepository[String],
  formTemplateService: FormTemplateService
)(implicit
  ec: ExecutionContext
) extends FormStatisticsAlgebra[Future] {
  override def getAllSavedVersions(): Future[AllSavedVersions] = {
    // db.forms.aggregate([
    //   {
    //     "$match": {
    //       "data.form.userId": {
    //         "$regex": "^(?!anonymous-session).*"
    //       }
    //     }
    //   },
    //   {
    //     "$group": {
    //       "_id": {
    //         "version": "$data.form.version"
    //       }
    //     }
    //   },
    //   {
    //     "$sort": {
    //       "_id.version": -1
    //     }
    //   },
    //   {
    //     "$group": {
    //       "_id": null,
    //       "stats": {
    //         "$push": "$_id.version"
    //       }
    //     }
    //   },
    //   {
    //     "$unset": [
    //       "_id"
    //     ]
    //   }
    //])

    val matchStage = Aggregates.filter(
      Filters.regex("data.form.userId", "^(?!anonymous-session).*")
    )

    val groupStage = Aggregates.group(
      equal("version", "$data.form.version")
    )

    val sortStage = Aggregates.sort(descending("_id.version"))

    val groupStage2 = Aggregates.group(
      null,
      Accumulators.push("stats", "$_id.version")
    )

    val unsetStage = BsonDocument("$unset" -> BsonArray("_id"))

    val pipeline: List[Bson] =
      List(matchStage, groupStage, sortStage, groupStage2, unsetStage)

    aggregate(pipeline)
      .map(_.map(Codecs.fromBson[AllSavedVersions]).headOption.getOrElse(AllSavedVersions.empty))

  }

  override def getSavedFormCount(formTemplateId: FormTemplateId): Future[Seq[VersionStats]] = {
    //db.forms.aggregate([
    //  {
    //    "$match": {
    //      "data.form.formTemplateId": "summary-section",
    //      "data.form.userId": {
    //        "$regex": "^(?!anonymous-session).*"
    //      }
    //    }
    //  },
    //  {
    //    "$addFields": {
    //      "isEmail": {
    //        "$regexMatch": {
    //          "input": "$data.form.userId",
    //          "regex": "^email"
    //        }
    //      }
    //    }
    //  },
    //  {
    //    "$group": {
    //      "_id": {
    //        "version": "$data.form.version",
    //        "isEmail": "$isEmail"
    //      },
    //      "count": {
    //        "$sum": 1
    //      }
    //    }
    //  },
    //  {
    //    "$set": {
    //      "isEmail": "$_id.isEmail",
    //      "version": "$_id.version"
    //    }
    //  },
    //  {
    //    "$unset": "_id"
    //  },
    //  {
    //    "$group": {
    //      "_id": "$version",
    //      "stats": {
    //        "$push": "$$ROOT"
    //      }
    //    }
    //  },
    //  {
    //    "$set": {
    //      "version": "$_id"
    //    }
    //  },
    //  {
    //    "$unset": [
    //      "stats.version",
    //      "_id"
    //    ]
    //  },
    //  {
    //    "$sort": {
    //      "version": -1
    //    }
    //  }
    //])

    val matchStage = Aggregates.filter(
      Updates.combine(
        Filters.equal("data.form.formTemplateId", formTemplateId.value),
        Filters.notEqual("data.form.status", Submitted.toString), // Ignore Submitted forms
        Filters.regex("data.form.userId", "^(?!anonymous-session).*") // Ignore annonymous forms
      )
    )

    val regexMatch = BsonDocument(
      "$regexMatch" -> BsonDocument(
        "input" -> "$data.form.userId",
        "regex" -> "^email"
      )
    )

    val addFieldsStage = Aggregates.addFields(Field("isEmail", regexMatch))

    val groupStage1 = Aggregates.group(
      Updates.combine(
        equal("version", "$data.form.version"),
        equal("isEmail", "$isEmail")
      ),
      sum("count", 1)
    )

    val setStage1 = Aggregates.set(
      Field("isEmail", "$_id.isEmail"),
      Field("version", "$_id.version")
    )

    val unsetStage1 = BsonDocument("$unset" -> BsonArray("_id"))

    val groupStage2 = Aggregates.group("$version", Accumulators.push("stats", "$$ROOT"))

    val setStage2 = Aggregates.set(
      Field("version", "$_id")
    )

    val unsetStage2 = BsonDocument("$unset" -> BsonArray("stats.version"))

    val sort = Aggregates.sort(descending("version"))

    val pipeline: List[Bson] =
      List(matchStage, addFieldsStage, groupStage1, setStage1, unsetStage1, groupStage2, setStage2, unsetStage2, sort)

    for {
      formTemplate <- formTemplateService.get(formTemplateId)
      res <- formTemplate.draftRetrievalMethod match {
               case NotPermitted => Future.successful(Seq.empty)
               case _            => aggregate(pipeline).map(_.map(Codecs.fromBson[VersionStats]))
             }
    } yield res
  }

  override def getSavedFormDetails(formTemplateId: FormTemplateId): Future[Seq[SavedFormDetail]] = {
    val filter = Aggregates.filter(
      and(
        equal("data.form.formTemplateId", formTemplateId.value),
        notEqual("data.form.status", Submitted.toString),
        gte("modifiedDetails.createdAt", LocalDate.now(ZoneId.of("Europe/London")).atStartOfDay().minusDays(30))
      )
    )

    val project = Aggregates.project(
      Updates.combine(
        excludeId(),
        computed(
          "createdDate",
          BsonDocument("$dateToString" -> Document("format" -> "%Y-%m-%d", "date" -> "$modifiedDetails.createdAt"))
        ),
        computed(
          "email",
          BsonDocument(
            "$cond" -> BsonArray(
              Document(
                "$regexMatch" -> Document("input" -> "$data.form.userId", "regex" -> "^email")
              ),
              1.toBson,
              0.toBson
            )
          )
        ),
        computed(
          "gg",
          BsonDocument(
            "$cond" -> BsonArray(
              Document(
                "$and" -> Seq(
                  Document(
                    "$regexMatch" -> Document("input" -> "$data.form.userId", "regex" -> "^(?!email).*")
                  ),
                  Document(
                    "$regexMatch" -> Document("input" -> "$data.form.userId", "regex" -> "^(?!anonymous-session).*")
                  )
                )
              ),
              1.toBson,
              0.toBson
            )
          )
        )
      )
    )
    val sort = Aggregates.sort(descending("createdDate"))

    val group = Aggregates.group(
      "$createdDate",
      first("createdDate", "$createdDate"),
      sum("emailCount", "$email"),
      sum("ggCount", "$gg")
    )

    val pipeline = List(filter, project, group, sort)

    aggregate(pipeline)
      .map(_.map(Codecs.fromBson[SavedFormDetail]))
  }

  def getSignedFormDetails(): Future[Seq[SignedFormDetails]] = {
    val filter = Aggregates.filter(
      equal("data.form.status", Signed.toString)
    )

    val project = Aggregates.project(
      Updates.combine(
        computed("lastUpdated", "$modifiedDetails.lastUpdated"),
        computed("envelopeId", "$data.form.envelopeId"),
        computed("formTemplateId", "$data.form.formTemplateId")
      )
    )

    val ignoreRecent = Aggregates.filter(
      lte(
        "lastUpdated",
        LocalDateTime.now(ZoneId.of("Europe/London")).minusMinutes(2)
      ) // Let's return forms which are older than 2 minutes
    )

    val sort = Aggregates.sort(ascending("lastUpdated"))

    val pipeline = List(filter, project, ignoreRecent, sort)

    aggregate(pipeline)
      .map(_.map(Codecs.fromBson[SignedFormDetails]))
  }

  private def aggregate(pipeline: Seq[Bson]): Future[Seq[BsonValue]] =
    formsCacheRepository.collection
      .aggregate[BsonValue](pipeline)
      .toFuture()
}
