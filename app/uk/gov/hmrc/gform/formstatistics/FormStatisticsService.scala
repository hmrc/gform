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
import org.mongodb.scala.bson.Document
import org.mongodb.scala.bson.{ BsonArray, BsonDocument }
import org.mongodb.scala.model.Accumulators.{ first, sum }
import org.mongodb.scala.model.{ Aggregates, Updates }
import org.mongodb.scala.model.Filters.{ and, equal, gte, lte, notEqual, regex }
import org.mongodb.scala.model.Projections.{ computed, excludeId }
import org.mongodb.scala.model.Sorts.{ ascending, descending }
import uk.gov.hmrc.gform.formtemplate.FormTemplateService
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, SavedForm, SavedFormDetail, Signed, SignedFormDetails, Submitted }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, NotPermitted }
import uk.gov.hmrc.mongo.play.json.Codecs
import uk.gov.hmrc.mongo.play.json.Codecs.JsonOps

import java.time.{ LocalDate, ZoneId }
import scala.concurrent.{ ExecutionContext, Future }

trait FormStatisticsAlgebra[F[_]] {
  def getSavedFormCount(formTemplateId: FormTemplateId): F[SavedForm]
  def getSavedFormDetails(formTemplateId: FormTemplateId): F[Seq[SavedFormDetail]]
  def getSignedFormDetails(): F[Seq[SignedFormDetails]]
}

class FormStatisticsService(formRepo: Repo[Form], formTemplateService: FormTemplateService)(implicit
  ec: ExecutionContext
) extends FormStatisticsAlgebra[Future] {

  override def getSavedFormCount(formTemplateId: FormTemplateId): Future[SavedForm] = {
    val baseQuery =
      and(equal("data.form.formTemplateId", formTemplateId.value), notEqual("data.form.status", Submitted.toString))

    val queryOfEmail = and(baseQuery, regex("data.form.userId", "^email"))

    val queryOfGG = and(
      baseQuery,
      and(regex("data.form.userId", "^(?!email).*"), regex("data.form.userId", "^(?!anonymous-session).*"))
    )

    for {
      formTemplate <- formTemplateService.get(formTemplateId)
      res <- formTemplate.draftRetrievalMethod match {
               case NotPermitted => Future.successful(SavedForm(formTemplateId, 0, 0))
               case _ =>
                 for {
                   countOfEmail <- formRepo.count(queryOfEmail)
                   countOfGG    <- formRepo.count(queryOfGG)
                 } yield SavedForm(formTemplateId, countOfEmail, countOfGG)
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
              1.toBson(),
              0.toBson()
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
              1.toBson(),
              0.toBson()
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

    formRepo
      .aggregate(pipeline)
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

    formRepo
      .aggregate(pipeline)
      .map(_.map(Codecs.fromBson[SignedFormDetails]))
  }
}
