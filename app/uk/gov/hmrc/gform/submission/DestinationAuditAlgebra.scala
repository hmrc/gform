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

import cats.Monad
import cats.instances.list._
import cats.instances.future._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.syntax.eq._
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{ ArrayNode, TextNode }
import org.joda.time.LocalDateTime
import play.api.libs.json._
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.form.{ BundledFormTreeNode, FormAlgebra }
import uk.gov.hmrc.gform.logging.Loggers
import uk.gov.hmrc.gform.repo.RepoAlgebra
import uk.gov.hmrc.gform.sharedmodel.{ PdfHtml, UserId, ValueClassFormat }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplate, FormTemplateId }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.json.ReactiveMongoFormats._

import scala.concurrent.ExecutionContext

case class SummaryHtmlId(value: UUID) extends AnyVal

object SummaryHtmlId {
  implicit val oformat: OFormat[SummaryHtmlId] =
    ValueClassFormat.oformat("summaryHtmlId", SummaryHtmlId.apply, _.value.toString)

  def apply(value: String): SummaryHtmlId = SummaryHtmlId(UUID.fromString(value))
}

case class SummaryHtml(id: SummaryHtmlId, summaryHtml: PdfHtml)

object SummaryHtml {
  implicit val format: OFormat[SummaryHtml] = new OFormat[SummaryHtml] {
    override def reads(json: JsValue): JsResult[SummaryHtml] =
      for {
        id          <- (json \ "id").validate[UUID]
        summaryHtml <- (json \ "summaryHtml").validate[String]
      } yield SummaryHtml(SummaryHtmlId(id), PdfHtml(summaryHtml))

    override def writes(summary: SummaryHtml): JsObject = {
      import summary._

      JsObject(
        Seq(
          "id"          -> JsString(id.value.toString),
          "hash"        -> JsNumber(summaryHtml.hashCode),
          "summaryHtml" -> JsString(summaryHtml.html)
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
  destinationResponseErrorBody: Option[String],
  workflowState: FormStatus,
  userId: UserId,
  caseworkerUserName: Option[String],
  parentFormSubmissionRefs: List[String],
  reviewData: Map[String, String],
  submissionRef: SubmissionRef,
  summaryHtmlId: SummaryHtmlId,
  id: UUID = UUID.randomUUID,
  timestamp: LocalDateTime = LocalDateTime.now)

object DestinationAudit {

  implicit val format: OFormat[DestinationAudit] = new OFormat[DestinationAudit] {

    override def reads(json: JsValue): JsResult[DestinationAudit] =
      for {
        formId                       <- (json \ "formId").validate[String].map(FormId(_))
        formTemplateId               <- (json \ "formTemplateId").validate[String].map(FormTemplateId(_))
        destinationId                <- (json \ "destinationId").validate[DestinationId]
        destinationType              <- (json \ "destinationType").validate[String]
        destinationResponseStatus    <- (json \ "destinationResponseStatus").validateOpt[Int]
        destinationResponseErrorBody <- (json \ "destinationResponseErrorBody").validateOpt[String]
        workflowState                <- readWorkflowState(json)
        userId                       <- (json \ "userId").validate[String].map(UserId(_))
        caseworkerUserName           <- (json \ "_caseworker_userName").validateOpt[String]
        parentFormSubmissionRefs     <- (json \ "parentFormSubmissionRefs").validateOpt[List[String]]
        submissionRef                <- (json \ "submissionRef").validate[String].map(SubmissionRef(_))
        summaryHtmlId                <- (json \ "summaryHtmlId").validate[String].map(SummaryHtmlId(_))
        id                           <- (json \ "id").validate[String].map(UUID.fromString)
        timestamp                    <- (json \ "timestamp").validate(localDateTimeRead)
        reviewData                   <- (json \ "reviewData").validateOpt[Map[String, String]]
      } yield
        DestinationAudit(
          formId,
          formTemplateId,
          destinationId,
          destinationType,
          destinationResponseStatus,
          destinationResponseErrorBody,
          workflowState,
          userId,
          caseworkerUserName,
          parentFormSubmissionRefs.getOrElse(Nil),
          reviewData.getOrElse(Map.empty),
          submissionRef,
          summaryHtmlId,
          id,
          timestamp
        )

    override def writes(audit: DestinationAudit): JsObject = {
      import audit._

      JsObject(
        Seq(
          Seq(
            "formId"                   -> JsString(formId.value),
            "formTemplateId"           -> JsString(formTemplateId.value),
            "destinationId"            -> JsString(destinationId.id),
            "destinationType"          -> JsString(destinationType),
            "workflowState"            -> JsString(workflowState.toString),
            "userId"                   -> JsString(userId.value),
            "id"                       -> JsString(id.toString),
            "timestamp"                -> localDateTimeWrite.writes(timestamp),
            "submissionRef"            -> JsString(submissionRef.value),
            "summaryHtmlId"            -> JsString(summaryHtmlId.value.toString),
            "parentFormSubmissionRefs" -> JsArray(parentFormSubmissionRefs.map(JsString)),
            "reviewData"               -> JsObject(reviewData.map { case (k, v) => k -> JsString(v) })
          ),
          destinationResponseStatus.map(s => "destinationResponseStatus"       -> JsNumber(s)).toSeq,
          destinationResponseErrorBody.map(s => "destinationResponseErrorBody" -> JsString(s)).toSeq,
          caseworkerUserName.map(c => "_caseworker_userName"                   -> JsString(c)).toSeq
        ).flatten)
    }
  }

  private def readWorkflowState(json: JsValue) =
    (json \ "workflowState")
      .validate[String]
      .flatMap(s => FormStatus.unapply(s).map(JsSuccess(_)).getOrElse(JsError(s"Invalid workflow state: $s")))
}

trait FormTreeAlgebra[M[_]] {
  def getFormTree(formId: FormId)(implicit hc: HeaderCarrier): M[Tree[BundledFormTreeNode]]
}

class DestinationAuditerFormTreeService[M[_]: Monad](auditAlgebra: DestinationAuditAlgebra[M])
    extends FormTreeAlgebra[M] {
  def getFormTree(rootFormId: FormId)(implicit hc: HeaderCarrier): M[Tree[BundledFormTreeNode]] =
    auditAlgebra.getLatestForForm(rootFormId).flatMap(buildHierarchyForAudit)

  private def buildDescendantTreesForChildAudits(childAudits: List[DestinationAudit]) =
    childAudits
      .traverse(buildHierarchyForAudit)

  private def buildDescendantTreesForSubmissionRef(submissionRef: SubmissionRef) =
    auditAlgebra.findLatestChildAudits(submissionRef).flatMap(buildDescendantTreesForChildAudits)

  private def buildHierarchyForAudit(audit: DestinationAudit): M[Tree[BundledFormTreeNode]] =
    buildDescendantTreesForSubmissionRef(audit.submissionRef)
      .map(
        Tree(BundledFormTreeNode(audit.formId, audit.submissionRef, audit.formTemplateId, audit.caseworkerUserName), _))
}

trait PdfSummaryAlgebra[M[_]] {
  def getLatestPdfHtml(formId: FormId)(implicit hc: HeaderCarrier): M[PdfHtml]
}

trait DestinationAuditAlgebra[M[_]] {
  def apply(
    destination: Destination,
    handlebarsDestinationResponseStatusCode: Option[Int],
    handlebarsDestinationResponseErrorBody: Option[String],
    formId: FormId,
    summaryHtml: PdfHtml,
    submissionReference: SubmissionRef,
    template: FormTemplate,
    model: HandlebarsTemplateProcessorModel)(implicit hc: HeaderCarrier): M[Unit]

  def auditForcedFormStatusChange(form: Form)(implicit hc: HeaderCarrier): M[Unit]

  def getLatestForForm(formId: FormId)(implicit hc: HeaderCarrier): M[DestinationAudit]

  def findLatestChildAudits(submissionRef: SubmissionRef): M[List[DestinationAudit]]
}

class RepoDestinationAuditer(
  auditRepository: RepoAlgebra[DestinationAudit, FOpt],
  summaryHtmlRepository: RepoAlgebra[SummaryHtml, FOpt],
  formAlgebra: FormAlgebra[FOpt])(implicit ec: ExecutionContext)
    extends DestinationAuditAlgebra[FOpt] with PdfSummaryAlgebra[FOpt] {
  def apply(
    destination: Destination,
    handlebarsDestinationResponseStatusCode: Option[Int],
    handlebarsDestinationResponseErrorBody: Option[String],
    formId: FormId,
    summaryHtml: PdfHtml,
    submissionReference: SubmissionRef,
    template: FormTemplate,
    model: HandlebarsTemplateProcessorModel)(implicit hc: HeaderCarrier): FOpt[Unit] = destination match {
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
                form.userId,
                getCaseworkerUsername(form.formData),
                getParentFormSubmissionRef(template, model),
                form.thirdPartyData.reviewData.getOrElse(Map.empty),
                submissionReference,
                summaryHtmlId
              ))
      } yield ()
  }

  private def apply(audit: DestinationAudit)(implicit hc: HeaderCarrier): FOpt[Unit] =
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
              form.userId,
              getCaseworkerUsername(form.formData),
              latestAudit.parentFormSubmissionRefs,
              form.thirdPartyData.reviewData.getOrElse(Map.empty),
              latestAudit.submissionRef,
              latestAudit.summaryHtmlId
            ))
    } yield ()

  def getLatestForForm(formId: FormId)(implicit hc: HeaderCarrier): FOpt[DestinationAudit] =
    auditRepository
      .search(DestinationAuditAlgebra.auditRepoFormIdSearch(formId))
      .subflatMap {
        _.sortWith((x, y) => x.timestamp.isAfter(y.timestamp)).headOption
          .toRight(UnexpectedState(s"Could not find any audits for form with ID ${formId.value}"))
      }

  def findLatestChildAudits(submissionRef: SubmissionRef): FOpt[List[DestinationAudit]] =
    auditRepository
      .search(
        Json.obj("parentFormSubmissionRefs" ->
          Json.obj("$in" -> Json.arr(JsString(submissionRef.value)))))
      .map {
        _.groupBy(_.formId).values.toList
          .flatMap(_.sortWith { case (x, y) => x.timestamp.isAfter(y.timestamp) }.headOption)
      }

  def getLatestPdfHtml(formId: FormId)(implicit hc: HeaderCarrier): FOpt[PdfHtml] =
    getLatestForForm(formId).flatMap { audit =>
      summaryHtmlRepository.get(audit.summaryHtmlId.value.toString).map(_.summaryHtml)
    }

  private def getCaseworkerUsername(formData: FormData): Option[String] =
    formData.find(FormComponentId("_caseworker_userName"))

  private def getParentFormSubmissionRef(
    template: FormTemplate,
    model: HandlebarsTemplateProcessorModel): List[String] = {
    import shapeless.syntax.typeable._
    import scala.collection.JavaConversions._

    def extractValuesFromTextOrArrayNode(node: JsonNode): List[String] =
      node.cast[TextNode].toList.map(_.asText) :::
        node.cast[ArrayNode].toList.flatMap(n => n.elements.toList.flatMap(extractValuesFromTextOrArrayNode))

    def extractValuesForFormComponent(id: FormComponentId): List[String] =
      Option(model.model.get(id.value)).toList.flatMap(extractValuesFromTextOrArrayNode)

    template.parentFormSubmissionRefs.flatMap(extractValuesForFormComponent)
  }

  private def findOrInsertSummaryHtml(summaryHtml: PdfHtml)(implicit hc: HeaderCarrier): FOpt[SummaryHtmlId] =
    findSummaryHtml(summaryHtml)
      .flatMap(_.fold(insertSummaryHtml(summaryHtml)) { _.id.pure[FOpt] })

  private def insertSummaryHtml(summaryHtml: PdfHtml): FOpt[SummaryHtmlId] = {
    val id = SummaryHtmlId(UUID.randomUUID)
    summaryHtmlRepository.upsert(SummaryHtml(id, summaryHtml)).map(_ => id)
  }

  private def findSummaryHtml(summaryHtml: PdfHtml): FOpt[Option[SummaryHtml]] =
    summaryHtmlRepository
      .search(Json.obj("hash" -> JsNumber(summaryHtml.hashCode)))
      .map(_.find(_.summaryHtml === summaryHtml))

  private def getDestinationType(destination: Destination): String = destination match {
    case _: Destination.HmrcDms           => Destination.hmrcDms
    case _: Destination.Composite         => Destination.composite
    case _: Destination.StateTransition   => Destination.stateTransition
    case d: Destination.HandlebarsHttpApi => s"${Destination.handlebarsHttpApi}.${d.profile.name}"
    case _                                => "Unknown"
  }
}

object DestinationAuditAlgebra {
  def auditRepoFormIdSearch(formId: FormId): JsObject = Json.obj("formId" -> JsString(formId.value))
}
