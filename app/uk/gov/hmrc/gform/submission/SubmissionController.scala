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

package uk.gov.hmrc.gform.submission

import cats.implicits._
import org.apache.commons.text.StringEscapeUtils
import play.api.libs.json.Json
import play.api.mvc.{ Action, AnyContent, ControllerComponents, Request }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormId, FormIdData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.{ SubmissionData, UserId }

import scala.concurrent.ExecutionContext

class SubmissionController(controllerComponents: ControllerComponents, submissionService: SubmissionService)(implicit
  ex: ExecutionContext
) extends BaseController(controllerComponents) {

  def createSubmission(formId: FormId, formTemplateId: FormTemplateId, envelopeId: EnvelopeId, noOfAttachments: Int) =
    formAction("createSubmission", formId) { implicit request =>
      submissionService
        .createSubmission(formId, formTemplateId, envelopeId, customerIdHeader, noOfAttachments)
        .fold(unexpectedState => BadRequest(unexpectedState.error), submission => Ok(Json.toJson(submission)))
    }

  def submitFormPlain(userId: UserId, formTemplateId: FormTemplateId): Action[SubmissionData] =
    submitFormByFormIdData(FormIdData.Plain(userId, formTemplateId))
  def submitForm(userId: UserId, formTemplateId: FormTemplateId, accessCode: AccessCode): Action[SubmissionData] =
    submitFormByFormIdData(FormIdData.WithAccessCode(userId, formTemplateId, accessCode))

  private def submitFormByFormIdData(formIdData: FormIdData): Action[SubmissionData] =
    formAction(parse.json[SubmissionData])("submitFormByFormIdData", formIdData) { implicit request =>
      import request._

      submissionService
        .submitForm(
          formIdData,
          customerIdHeader,
          body
        )
        .fold(unexpectedState => BadRequest(unexpectedState.error), _ => NoContent)
    }

  def submissionDetailsPlain(
    userId: UserId,
    formTemplateId: FormTemplateId,
    envelopeId: EnvelopeId
  ): Action[AnyContent] =
    submissionDetailsFormIdData(FormIdData.Plain(userId, formTemplateId), envelopeId)
  def submissionDetails(
    userId: UserId,
    formTemplateId: FormTemplateId,
    accessCode: AccessCode,
    envelopeId: EnvelopeId
  ): Action[AnyContent] =
    submissionDetailsFormIdData(FormIdData.WithAccessCode(userId, formTemplateId, accessCode), envelopeId)

  private def submissionDetailsFormIdData(formIdData: FormIdData, envelopeId: EnvelopeId): Action[AnyContent] =
    formAction("submissionDetailsFormIdData", formIdData) { _ =>
      submissionService
        .submissionDetails(SubmissionId(formIdData.toFormId, envelopeId))
        .map {
          case Some(details) => Ok(Json.toJson(details))
          case None          => NotFound
        }
    }

  def retrieveAll(formTemplateId: FormTemplateId, page: Int, pageSize: Int): Action[AnyContent] = Action.async { _ =>
    submissionService
      .submissionPageDetails(formTemplateId, page, pageSize)
      .map(submissions => Ok(Json.toJson(submissions)))
  }

  def customerIdHeader(implicit request: Request[_]): String =
    request.headers
      .get("customerId")
      .map(StringEscapeUtils.unescapeHtml4)
      .getOrElse("")
}
