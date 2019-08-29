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

package uk.gov.hmrc.gform.submission.destinations

import java.util.UUID

import org.joda.time.LocalDateTime
import play.api.libs.json.{ JsArray, JsError, JsNumber, JsObject, JsResult, JsString, JsSuccess, JsValue, OFormat }
import uk.gov.hmrc.gform.sharedmodel.{ SubmissionRef, UserId }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormId, FormStatus }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.mongo.json.ReactiveMongoFormats._

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
