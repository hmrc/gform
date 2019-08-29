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
import uk.gov.hmrc.gform.sharedmodel.form.{ FormId, FormStatus }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils._
import uk.gov.hmrc.gform.sharedmodel.BundledFormSubmissionData

import scala.concurrent.{ ExecutionContext, Future }

class FormBundleController(oFormBundleSubmissionService: Option[FormBundleSubmissionService[FOpt]])(
  implicit ex: ExecutionContext)
    extends BaseController {
  def getFormBundle(rootFormId: FormId): Action[AnyContent] =
    Action.async { implicit request =>
      oFormBundleSubmissionService.fold(
        Future.successful(BadRequest("Can't getFormTree. No FormBundleSubmissionService has been configured."))) {
        formBundleSubmissionService =>
          Logger.info(s"getFormBundle, formId: '${rootFormId.value}, ${loggingHelpers.cleanHeaders(request.headers)}")

          formBundleSubmissionService
            .formTree(rootFormId)
            .map(_.map(_.formId).toList)
            .toFuture
            .asOkJson
      }
    }

  def submitFormBundleAfterReview(rootFormId: FormId): Action[NonEmptyList[BundledFormSubmissionData]] =
    Action.async(parse.json[NonEmptyList[BundledFormSubmissionData]]) { implicit request =>
      oFormBundleSubmissionService.fold(
        Future.successful(
          BadRequest("Can't submitFormBundleAfterReview. No FormBundleSubmissionService has been configured."))) {
        formBundleSubmissionService =>
          Logger.info(
            s"submitFormBundleAfterReview, formId: ${rootFormId.value}, ${loggingHelpers.cleanHeaders(request.headers)}")

          import request._

          formBundleSubmissionService
            .submitFormBundleAfterReview(rootFormId, body)
            .fold(unexpectedState => BadRequest(unexpectedState.error), _ => NoContent)
      }
    }

  def forceUpdateFormStatus(formId: FormId, status: FormStatus): Action[AnyContent] = Action.async { implicit request =>
    oFormBundleSubmissionService.fold(
      Future
        .successful(BadRequest("Can't forceUpdateFormStatus. No FormBundleSubmissionService has been configured."))) {
      formBundleSubmissionService =>
        Logger.info(s"forceUpdateFormStatus, ${formId.value}, $status, ${loggingHelpers.cleanHeaders(request.headers)}")
        formBundleSubmissionService
          .forceUpdateFormStatus(formId, status)
          .fold(unexpectedState => BadRequest(unexpectedState.error), _ => NoContent)
    }
  }
}
