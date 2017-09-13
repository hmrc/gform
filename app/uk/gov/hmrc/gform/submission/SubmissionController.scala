/*
 * Copyright 2017 HM Revenue & Customs
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

import play.api.mvc.Action
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel._
import cats.data._
import cats.implicits._
import play.api.Logger
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.play.http.NotImplementedException

import scala.concurrent.ExecutionContext.Implicits.global

class SubmissionController(
    submissionService: SubmissionService
) extends BaseController {

  def submit(formId: FormId) = Action.async { implicit request =>
    Logger.info(s"submit, formId: '${formId.value}, Headers: '${request.headers.remove("Authorization", "token")}'")
    //TODO check form status. If after submission don't call this function
    //TODO authentication
    //TODO authorisation
    //TODO validate all sections before submission (whole form)
    //TODO change status of form to 'submitted'

    submissionService.submission(formId).fold(
      _.asBadRequest,
      _ => NoContent
    )
  }

  def submissionStatus(formId: FormId) = Action.async { implicit request =>
    Logger.info(s"checking submission status, formId: '${formId.value}, Headers: '${request.headers.remove("Authorization", "token")}'")
    //TODO check form status. If after submission don't call this function
    //TODO authentication
    //TODO authorisation

    throw new NotImplementedException("Bear with me, work in progress")
  }

}
