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

import cats.implicits._
import play.api.Logger
import play.api.mvc.{ Action, AnyContent }
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil.toAffinityGroupO
import uk.gov.hmrc.gform.sharedmodel.form.FormIdData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.{ SubmissionData, UserId }

import scala.concurrent.ExecutionContext

class SubmissionController(submissionService: SubmissionService)(implicit ex: ExecutionContext) extends BaseController {

  def submitFormPlain(userId: UserId, formTemplateId: FormTemplateId): Action[SubmissionData] =
    submitFormByFormIdData(FormIdData.Plain(userId, formTemplateId))
  def submitForm(userId: UserId, formTemplateId: FormTemplateId, accessCode: AccessCode): Action[SubmissionData] =
    submitFormByFormIdData(FormIdData.WithAccessCode(userId, formTemplateId, accessCode))

  private def submitFormByFormIdData(formIdData: FormIdData): Action[SubmissionData] =
    Action.async(parse.json[SubmissionData]) { implicit request =>
      Logger.info(s"submitForm, formId: '${formIdData.toFormId.value}, ${loggingHelpers.cleanHeaders(request.headers)}")

      import request._

      submissionService
        .submitForm(
          formIdData,
          headers.get("customerId").getOrElse(""),
          toAffinityGroupO(headers.get("affinityGroup")),
          body
        )
        .fold(unexpectedState => BadRequest(unexpectedState.error), _ => NoContent)
    }

  def submissionDetailsPlain(userId: UserId, formTemplateId: FormTemplateId): Action[AnyContent] =
    submissionDetailsFormIdData(FormIdData.Plain(userId, formTemplateId))
  def submissionDetails(userId: UserId, formTemplateId: FormTemplateId, accessCode: AccessCode): Action[AnyContent] =
    submissionDetailsFormIdData(FormIdData.WithAccessCode(userId, formTemplateId, accessCode))

  private def submissionDetailsFormIdData(formIdData: FormIdData): Action[AnyContent] =
    Action.async { implicit request =>
      Logger.info(
        s"submissionDetails, formId: '${formIdData.toFormId.value}, ${loggingHelpers.cleanHeaders(request.headers)}")

      submissionService.submissionDetails(formIdData).asOkJson
    }
}
