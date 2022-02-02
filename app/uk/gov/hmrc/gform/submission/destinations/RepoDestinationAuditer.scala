/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.submission.destinations

import java.util.UUID

import cats.instances.future._
import cats.syntax.applicative._
import cats.syntax.eq._
import cats.syntax.flatMap._
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{ ArrayNode, TextNode }
import org.mongodb.scala.model.Filters
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.logging.Loggers
import uk.gov.hmrc.gform.repo.RepoAlgebra
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplate }
import uk.gov.hmrc.gform.sharedmodel.{ PdfHtml, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormData, FormId }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext

class RepoDestinationAuditer(
  auditRepository: RepoAlgebra[DestinationAudit, FOpt],
  summaryHtmlRepository: RepoAlgebra[SummaryHtml, FOpt],
  formAlgebra: FormAlgebra[FOpt]
)(implicit ec: ExecutionContext)
    extends DestinationAuditAlgebra[FOpt] with PdfSummaryAlgebra[FOpt] {
  def apply(
    destination: Destination,
    handlebarsDestinationResponseStatusCode: Option[Int],
    handlebarsDestinationResponseErrorBody: Option[String],
    formId: FormId,
    summaryHtml: PdfHtml,
    submissionReference: SubmissionRef,
    template: FormTemplate,
    model: HandlebarsTemplateProcessorModel
  )(implicit hc: HeaderCarrier): FOpt[Unit] = destination match {
    case _: Destination.Composite => ().pure[FOpt]
    case _ =>
      for {
        form          <- formAlgebra.get(formId)
        summaryHtmlId <- findOrInsertSummaryHtml(summaryHtml)
        _ <- apply(
               DestinationAudit(
                 formId,
                 form.formTemplateId,
                 destination.id,
                 getDestinationType(destination),
                 handlebarsDestinationResponseStatusCode,
                 handlebarsDestinationResponseErrorBody,
                 form.status,
                 form.sensitive.userId,
                 getCaseworkerUsername(form.sensitive.formData),
                 getParentFormSubmissionRef(template, model),
                 form.sensitive.thirdPartyData.reviewData.getOrElse(Map.empty),
                 submissionReference,
                 summaryHtmlId
               )
             )
      } yield ()
  }

  private def apply(audit: DestinationAudit): FOpt[Unit] =
    success(Loggers.destinations.info(s"Destination audit: ${DestinationAudit.format.writes(audit)}")) >>
      auditRepository.upsert(audit) >>
      success(())

  def auditForcedFormStatusChange(form: Form)(implicit hc: HeaderCarrier): FOpt[Unit] =
    for {
      latestAudit <- getLatestForForm(form._id)
      _ <- apply(
             DestinationAudit(
               form._id,
               form.formTemplateId,
               DestinationId("forcedFormStatusChange"),
               "forcedFormStatusChange",
               None,
               None,
               form.status,
               form.sensitive.userId,
               getCaseworkerUsername(form.sensitive.formData),
               latestAudit.parentFormSubmissionRefs,
               form.sensitive.thirdPartyData.reviewData.getOrElse(Map.empty),
               latestAudit.submissionRef,
               latestAudit.summaryHtmlId
             )
           )
    } yield ()

  def getLatestForForm(formId: FormId)(implicit hc: HeaderCarrier): FOpt[DestinationAudit] =
    auditRepository
      .search(DestinationAuditAlgebra.auditRepoFormIdSearch(formId), DestinationAuditAlgebra.latestTimestampFirst)
      .subflatMap {
        _.headOption
          .toRight(UnexpectedState(s"Could not find any audits for form with ID ${formId.value}"))
      }

  def findLatestChildAudits(submissionRef: SubmissionRef): FOpt[List[DestinationAudit]] =
    auditRepository
      .search(DestinationAuditAlgebra.auditRepoLatestChildAuditsSearch(submissionRef))
      .map {
        _.groupBy(_.formId).values.toList
          .flatMap(getLatest)
      }

  def getLatest(audits: List[DestinationAudit]): Option[DestinationAudit] =
    audits
      .sortWith((x, y) => x.timestamp.isAfter(y.timestamp))
      .headOption

  def getLatestPdfHtml(formId: FormId)(implicit hc: HeaderCarrier): FOpt[PdfHtml] =
    getLatestForForm(formId).flatMap { audit =>
      summaryHtmlRepository.get(audit.summaryHtmlId.value.toString).map(_.summaryHtml)
    }

  private def getCaseworkerUsername(formData: FormData): Option[String] =
    formData.find(FormComponentId("_caseworker_userName"))

  private def getParentFormSubmissionRef(
    template: FormTemplate,
    model: HandlebarsTemplateProcessorModel
  ): List[String] = {
    import shapeless.syntax.typeable._

    import scala.collection.JavaConverters._

    def extractValuesFromTextOrArrayNode(node: JsonNode): List[String] =
      node.cast[TextNode].toList.map(_.asText) :::
        node.cast[ArrayNode].toList.flatMap(n => n.elements.asScala.flatMap(extractValuesFromTextOrArrayNode))

    def extractValuesForFormComponent(id: FormComponentId): List[String] =
      Option(model.model.get(id.value)).toList.flatMap(extractValuesFromTextOrArrayNode)

    template.parentFormSubmissionRefs.flatMap(extractValuesForFormComponent)
  }

  private def findOrInsertSummaryHtml(summaryHtml: PdfHtml): FOpt[SummaryHtmlId] =
    findSummaryHtml(summaryHtml)
      .flatMap(_.fold(insertSummaryHtml(summaryHtml))(_.id.pure[FOpt]))

  private def insertSummaryHtml(summaryHtml: PdfHtml): FOpt[SummaryHtmlId] = {
    val id = SummaryHtmlId(UUID.randomUUID)
    summaryHtmlRepository.upsert(SummaryHtml(id, summaryHtml)).map(_ => id)
  }

  private def findSummaryHtml(summaryHtml: PdfHtml): FOpt[Option[SummaryHtml]] =
    summaryHtmlRepository
      .search(Filters.equal("hash", summaryHtml.hashCode))
      .map(_.find(_.summaryHtml === summaryHtml))

  private def getDestinationType(destination: Destination): String = destination match {
    case _: Destination.HmrcDms           => Destination.hmrcDms
    case _: Destination.Composite         => Destination.composite
    case _: Destination.StateTransition   => Destination.stateTransition
    case d: Destination.HandlebarsHttpApi => s"${Destination.handlebarsHttpApi}.${d.profile.name}"
    case _                                => "Unknown"
  }
}
