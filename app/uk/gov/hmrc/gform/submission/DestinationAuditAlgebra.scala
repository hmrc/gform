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
import org.joda.time.LocalDateTime
import play.api.libs.json._
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.form.{ BundledFormTreeNode, FormAlgebra }
import uk.gov.hmrc.gform.logging.Loggers
import uk.gov.hmrc.gform.repo.RepoAlgebra
import uk.gov.hmrc.gform.sharedmodel.{ PdfHtml, UserId, ValueClassFormat }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId }
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
  workflowState: FormStatus,
  userId: UserId,
  caseworkerUserName: Option[String],
  parentFormSubmissionRef: Option[String],
  reviewData: Map[String, String],
  submissionRef: SubmissionRef,
  summaryHtmlId: SummaryHtmlId,
  id: UUID = UUID.randomUUID,
  timestamp: LocalDateTime = LocalDateTime.now)

object DestinationAudit {

  implicit val format: OFormat[DestinationAudit] = new OFormat[DestinationAudit] {

    override def reads(json: JsValue): JsResult[DestinationAudit] =
      for {
        formId                    <- (json \ "formId").validate[String].map(FormId(_))
        formTemplateId            <- (json \ "formTemplateId").validate[String].map(FormTemplateId(_))
        destinationId             <- (json \ "destinationId").validate[DestinationId]
        destinationType           <- (json \ "destinationType").validate[String]
        destinationResponseStatus <- (json \ "destinationResponseStatus").validateOpt[Int]
        workflowState             <- readWorkflowState(json)
        userId                    <- (json \ "userId").validate[String].map(UserId(_))
        caseworkerUserName        <- (json \ "caseworkerUserName").validateOpt[String]
        parentFormSubmissionRef   <- (json \ "_parentFormSubmissionRef").validateOpt[String]
        reviewData                <- (json \ "reviewData").validate[Map[String, String]]
        submissionRef             <- (json \ "submissionRef").validate[String].map(SubmissionRef(_))
        summaryHtmlId             <- (json \ "summaryHtmlId").validate[String].map(SummaryHtmlId(_))
        id                        <- (json \ "id").validate[String].map(UUID.fromString)
        timestamp                 <- (json \ "timestamp").validate[LocalDateTime]
      } yield
        DestinationAudit(
          formId,
          formTemplateId,
          destinationId,
          destinationType,
          destinationResponseStatus,
          workflowState,
          userId,
          caseworkerUserName,
          parentFormSubmissionRef,
          reviewData,
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
            "formId"          -> JsString(formId.value),
            "formTemplateId"  -> JsString(formTemplateId.value),
            "destinationId"   -> JsString(destinationId.id),
            "destinationType" -> JsString(destinationType),
            "workflowState"   -> JsString(workflowState.toString),
            "userId"          -> JsString(userId.value),
            "id"              -> JsString(id.toString),
            "timestamp"       -> localDateTimeWrite.writes(timestamp),
            "submissionRef"   -> JsString(submissionRef.value),
            "summaryHtmlId"   -> JsString(summaryHtmlId.value.toString)
          ),
          destinationResponseStatus.map(s => "destinationResponseStatus" -> JsNumber(s)).toSeq,
          caseworkerUserName.map(c => "_caseworker_userName"             -> JsString(c)).toSeq,
          parentFormSubmissionRef.map(c => "_parentFormSubmissionRef"    -> JsString(c)).toSeq,
          workflowState match {
            case Returning => reviewData.map { case (k, v) => k -> JsString(v) }
            case _         => Seq.empty
          }
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
      .map(Tree(BundledFormTreeNode(audit.formId, audit.submissionRef, audit.formTemplateId), _))
}

trait PdfSummaryAlgebra[M[_]] {
  def getLatestPdfHtml(formId: FormId)(implicit hc: HeaderCarrier): M[PdfHtml]
}

trait DestinationAuditAlgebra[M[_]] {
  def apply(
    destination: Destination,
    handlebarsDestinationResponseStatusCode: Option[Int],
    formId: FormId,
    summaryHtml: PdfHtml,
    submissionReference: SubmissionRef)(implicit hc: HeaderCarrier): M[Unit]

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
    formId: FormId,
    summaryHtml: PdfHtml,
    submissionReference: SubmissionRef)(implicit hc: HeaderCarrier): FOpt[Unit] = destination match {
    case _: Destination.Composite => ().pure[FOpt]
    case _ =>
      for {
        form          <- formAlgebra.get(formId)
        summaryHtmlId <- findOrInsertSummaryHtml(summaryHtml)
        audit = DestinationAudit(
          formId,
          form.formTemplateId,
          destination.id,
          getDestinationType(destination),
          handlebarsDestinationResponseStatusCode,
          form.status,
          form.userId,
          getCaseworkerUsername(form.formData),
          getParentFormSubmissionRef(form.formData),
          form.thirdPartyData.reviewData.getOrElse(Map.empty),
          submissionReference,
          summaryHtmlId
        )
        _ = Loggers.destinations.info(s"Destination audit: ${DestinationAudit.format.writes(audit)}")
        _ <- auditRepository.upsert(audit)
      } yield ()
  }

  def getLatestForForm(formId: FormId)(implicit hc: HeaderCarrier): FOpt[DestinationAudit] =
    auditRepository
      .search(Json.obj("formId" -> JsString(formId.value)))
      .subflatMap {
        _.sortWith((x, y) => x.timestamp.isAfter(y.timestamp)).headOption
          .toRight(UnexpectedState(s"Could not find any audits for form with ID ${formId.value}"))
      }

  def findLatestChildAudits(submissionRef: SubmissionRef): FOpt[List[DestinationAudit]] =
    auditRepository
      .search(Json.obj("_parentFormSubmissionRef" -> JsString(submissionRef.value)))
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

  private def getParentFormSubmissionRef(formData: FormData): Option[String] =
    formData.find(FormComponentId("_parentFormSubmissionRef"))

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
