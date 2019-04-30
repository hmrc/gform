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
import play.api.mvc.{ Action, AnyContent, Request }
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil._
import uk.gov.hmrc.gform.sharedmodel.SubmissionData
import uk.gov.hmrc.gform.sharedmodel.form.FormId

import scala.concurrent.ExecutionContext

class SubmissionController(submissionService: SubmissionService)(implicit ex: ExecutionContext) extends BaseController {

  def submitWithPdf(formId: FormId): Action[SubmissionData] = Action.async(parse.json[SubmissionData]) {
    implicit request =>
      Logger.info(s"submit, formId: '${formId.value}, ${loggingHelpers.cleanHeaders(request.headers)}")

      submissionService
        .submissionWithPdf(
          formId,
          request.headers.get("customerId").getOrElse(""),
          toAffinityGroupO(request.headers.get("affinityGroup")),
          request.body
        )
        .fold(_.asBadRequest, _ => NoContent)
  }

  def submissionStatus(formId: FormId): Action[AnyContent] = Action.async { implicit request =>
    Logger.info(
      s"checking submission status, formId: '${formId.value}, ${loggingHelpers.cleanHeaders(request.headers)}")
    //TODO authentication
    //TODO authorisation

    submissionService.submissionDetails(formId).asOkJson
  }
}
