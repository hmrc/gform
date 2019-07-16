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

package uk.gov.hmrc.gform.submission
import java.util.UUID

import cats.Applicative
import cats.syntax.applicative._
import cats.syntax.eq._
import cats.instances.future._
import cats.instances.string._
import org.joda.time.LocalDateTime
import play.api.libs.json._
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.json.ReactiveMongoFormats.localDateTimeWrite

import scala.concurrent.ExecutionContext

case class SummaryHtmlId(value: UUID) extends AnyVal

case class SummaryHtml(id: SummaryHtmlId, summaryHtml: String)

object SummaryHtml {
  implicit val format: OFormat[SummaryHtml] = new OFormat[SummaryHtml] {
    override def reads(json: JsValue): JsResult[SummaryHtml] =
      for {
        id          <- (json \ "id").validate[UUID]
        summaryHtml <- (json \ "summaryHtml").validate[String]
      } yield SummaryHtml(SummaryHtmlId(id), summaryHtml)

    override def writes(summary: SummaryHtml): JsObject = {
      import summary._

      JsObject(
        Seq(
          "id"          -> JsString(id.value.toString),
          "hash"        -> JsNumber(summaryHtml.hashCode),
          "summaryHtml" -> JsString(summaryHtml)
        ))
    }
  }
}

case class DestinationAudit(
  formId: FormId,
  formTemplateId: FormTemplateId,
  destinationId: DestinationId,
  destinationType: String,
  destinationResponseStatus: Option[Int],
  workflowState: FormStatus,
  userId: UserId,
  caseworkerUserName: Option[String],
  submissionReference: SubmissionRef,
  summaryHtmlId: SummaryHtmlId,
  id: UUID = UUID.randomUUID,
  timestamp: LocalDateTime = LocalDateTime.now)

object DestinationAudit {
  implicit val format: OFormat[DestinationAudit] = new OFormat[DestinationAudit] {
    override def reads(json: JsValue): JsResult[DestinationAudit] = throw new Exception("Haven't implemented reads yet")
    override def writes(audit: DestinationAudit): JsObject = {
      import audit._

      JsObject(
        Seq(
          Seq(
            "formId"          -> JsString(formId.value),
            "formTemplateId"  -> JsString(formTemplateId.value),
            "destinationId"   -> JsString(destinationId.id),
            "destinationType" -> JsString(destinationType),
            "workflowState"   -> JsString(workflowState.toString),
            "userId"          -> JsString(userId.value),
            "id"              -> JsString(id.toString),
            "timestamp"       -> localDateTimeWrite.writes(timestamp),
            "submissionRef"   -> JsString(submissionReference.value),
            "summaryHtmlId"   -> JsString(summaryHtmlId.value.toString)
          ),
          destinationResponseStatus.map(s => "destinationResponseStatus" -> JsNumber(s)).toSeq,
          caseworkerUserName.map(c => "_caseworker_userName"             -> JsString(c)).toSeq
        ).flatten)
    }
  }
}

trait DestinationAuditAlgebra[M[_]] {
  def apply(
    destination: Destination,
    handlebarsDestinationResponseStatusCode: Option[Int],
    formId: FormId,
    summaryHtml: String,
    submissionReference: SubmissionRef)(implicit hc: HeaderCarrier): M[Unit]
}

class RepoDestinationAuditer(
  auditRepository: Repo[DestinationAudit],
  summaryHtmlRepository: Repo[SummaryHtml],
  formAlgebra: FormAlgebra[FOpt])(implicit ec: ExecutionContext)
    extends DestinationAuditAlgebra[FOpt] {
  def apply(
    destination: Destination,
    handlebarsDestinationResponseStatusCode: Option[Int],
    formId: FormId,
    summaryHtml: String,
    submissionReference: SubmissionRef)(implicit hc: HeaderCarrier): FOpt[Unit] =
    for {
      form          <- formAlgebra.get(formId)
      summaryHtmlId <- findOrInsertSummaryHtml(summaryHtml)
      _ <- auditRepository.upsert(
            new DestinationAudit(
              formId,
              form.formTemplateId,
              destination.id,
              destination.getClass.getSimpleName,
              handlebarsDestinationResponseStatusCode,
              form.status,
              form.userId,
              getCaseworkerUsername(form.formData),
              submissionReference,
              summaryHtmlId
            ))
    } yield ()

  private def getCaseworkerUsername(formData: FormData): Option[String] =
    formData.find(FormComponentId("_caseworker_userName"))

  private def findOrInsertSummaryHtml(summaryHtml: String)(implicit hc: HeaderCarrier): FOpt[SummaryHtmlId] =
    findSummaryHtml(summaryHtml)
      .flatMap(_.fold(insertSummaryHtml(summaryHtml)) { _.id.pure[FOpt] })

  private def insertSummaryHtml(summaryHtml: String): FOpt[SummaryHtmlId] = {
    val id = SummaryHtmlId(UUID.randomUUID)
    summaryHtmlRepository.upsert(SummaryHtml(id, summaryHtml)).map(_ => id)
  }

  private def findSummaryHtml(summaryHtml: String): FOpt[Option[SummaryHtml]] = fromFutureA {
    summaryHtmlRepository
      .search(
        JsObject(
          Seq(
            "hash" -> JsNumber(summaryHtml.hashCode)
          )))
      .map(_.find(_.summaryHtml === summaryHtml))
  }

}

class NullDestinationAuditer[M[_]: Applicative] extends DestinationAuditAlgebra[M] {
  override def apply(
    destination: Destination,
    handlebarsDestinationResponseStatusCode: Option[Int],
    formId: FormId,
    summaryHtml: String,
    submissionReference: SubmissionRef)(implicit hc: HeaderCarrier): M[Unit] = ().pure
}
