/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.dms

import play.api.Logger
import play.api.libs.Files
import play.api.libs.Files.TemporaryFile
import play.api.libs.json.JsValue
import play.api.mvc.{ Action, ControllerComponents, MultipartFormData, Request }
import uk.gov.hmrc.gform.controllers.BaseController

import scala.concurrent.{ ExecutionContext, Future }

class DmsSubmissionController(
  controllerComponents: ControllerComponents,
  dmsSubmissionAlgebra: DmsSubmissionAlgebra[Future])(implicit ex: ExecutionContext)
    extends BaseController(controllerComponents) {

  def submitToDms: Action[JsValue] = Action.async(parse.json) { implicit request =>
    withJsonBody[DmsHtmlSubmission] { dmsHtmlSubmission =>
      dmsSubmissionAlgebra.submitToDms(dmsHtmlSubmission, List.empty).map(id => Ok(id.value))
    }
  }

  def submitToDmsWithAttachments: Action[MultipartFormData[Files.TemporaryFile]] =
    Action.async(parse.multipartFormData) { implicit request: Request[MultipartFormData[TemporaryFile]] =>
      DmsSubmissionWithAttachmentsRequestInterpreter(request) match {
        case Right((dmsHtmlSubmission, fileAttachments)) =>
          dmsSubmissionAlgebra
            .submitToDms(dmsHtmlSubmission, fileAttachments)
            .map(id => Ok(id.value))
        case Left(message) =>
          Logger.info(message)
          Future.successful(BadRequest(message))
      }
    }

  /**
    * This endpoint is not used by gform but its here for other services to leverage our backend integration with DMS via FUaaS
    * Its currently used by Overseas Agents team
    */
  def submitPdfToDms: Action[MultipartFormData[Files.TemporaryFile]] = Action.async(parse.multipartFormData) {
    implicit request: Request[MultipartFormData[TemporaryFile]] =>
      DmsSubmissionRequestInterpreter(request) match {
        case Right((pdfBytes, metadata)) =>
          dmsSubmissionAlgebra
            .submitPdfToDms(pdfBytes, metadata, List.empty)
            .map(id => Ok(id.value))
        case Left(message) =>
          Logger.info(message)
          Future.successful(BadRequest(message))
      }
  }
}
