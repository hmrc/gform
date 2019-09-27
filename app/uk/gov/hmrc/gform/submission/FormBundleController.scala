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

import cats.data.NonEmptyList
import cats.implicits._
import play.api.Logger
import play.api.mvc.{ Action, AnyContent }
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, UserId }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormIdData, FormStatus }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils._
import uk.gov.hmrc.gform.sharedmodel.BundledFormSubmissionData

import scala.concurrent.{ ExecutionContext, Future }

class FormBundleController(oFormBundleSubmissionService: Option[FormBundleSubmissionService[FOpt]])(
  implicit ex: ExecutionContext)
    extends BaseController {
  def getFormBundle(userId: UserId, formTemplateId: FormTemplateId, accessCode: AccessCode): Action[AnyContent] =
    Action.async { implicit request =>
      oFormBundleSubmissionService.fold(
        Future.successful(BadRequest("Can't getFormTree. No FormBundleSubmissionService has been configured."))) {
        formBundleSubmissionService =>
          val rootFormIdData = FormIdData.WithAccessCode(userId, formTemplateId, accessCode)

          Logger.info(
            s"getFormBundle, formId: '${rootFormIdData.toFormId.value}, ${loggingHelpers.cleanHeaders(request.headers)}")

          formBundleSubmissionService
            .formTree(rootFormIdData)
            .map(_.map(_.formIdData).toList)
            .toFuture
            .asOkJson
      }
    }

  def submitFormBundleAfterReview(
    userId: UserId,
    formTemplateId: FormTemplateId,
    accessCode: AccessCode): Action[NonEmptyList[BundledFormSubmissionData]] =
    Action.async(parse.json[NonEmptyList[BundledFormSubmissionData]]) { implicit request =>
      oFormBundleSubmissionService.fold(
        Future.successful(
          BadRequest("Can't submitFormBundleAfterReview. No FormBundleSubmissionService has been configured."))) {
        formBundleSubmissionService =>
          val rootFormIdData = FormIdData.WithAccessCode(userId, formTemplateId, accessCode)

          Logger.info(s"submitFormBundleAfterReview, formId: ${rootFormIdData.toFormId.value}, ${loggingHelpers
            .cleanHeaders(request.headers)}")

          import request._

          formBundleSubmissionService
            .submitFormBundleAfterReview(rootFormIdData, body)
            .fold(unexpectedState => BadRequest(unexpectedState.error), _ => NoContent)
      }
    }

  def forceUpdateFormStatus(
    userId: UserId,
    formTemplateId: FormTemplateId,
    accessCode: AccessCode,
    status: FormStatus): Action[AnyContent] = Action.async { implicit request =>
    oFormBundleSubmissionService.fold(
      Future
        .successful(BadRequest("Can't forceUpdateFormStatus. No FormBundleSubmissionService has been configured."))) {
      formBundleSubmissionService =>
        val formIdData = FormIdData.WithAccessCode(userId, formTemplateId, accessCode)
        Logger.info(
          s"forceUpdateFormStatus, ${formIdData.toFormId.value}, $status, ${loggingHelpers.cleanHeaders(request.headers)}")
        formBundleSubmissionService
          .forceUpdateFormStatus(formIdData.toFormId, status)
          .fold(unexpectedState => BadRequest(unexpectedState.error), _ => NoContent)
    }
  }
}
