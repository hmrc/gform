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

package uk.gov.hmrc.gform.controllers

import cats.instances.future._
import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc.{ Action, AnyContent, RequestHeader }
import uk.gov.hmrc.gform.connectors.Save4LaterConnector
import uk.gov.hmrc.gform.core.ServiceResponse
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.repositories.{ AbstractRepo, SubmissionRepository }
import uk.gov.hmrc.gform.services.{ FormService, SubmissionService, UpdateOperation, UpdateTolerantOperation, _ }
import uk.gov.hmrc.gform.typeclasses.{ FusFeUrl, FusUrl, ServiceUrl }
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import uk.gov.hmrc.play.microservice.controller.BaseController

import scala.concurrent.Future

class FormController()(
    implicit
    formRepo: AbstractRepo[Form],
    s4l: Save4LaterConnector,
    formTemplateRepo: AbstractRepo[FormTemplate],
    submissionRepo: SubmissionRepository,
    fusUrl: ServiceUrl[FusUrl],
    fusFeUrl: ServiceUrl[FusFeUrl]
) extends BaseController {

  def newForm(userId: UserId, formTypeId: FormTypeId) = Action.async { implicit request =>

    val t: ServiceResponse[FormTemplate] = FormTemplateService.get(formTypeId)
    val e: ServiceResponse[EnvelopeId] = FileUploadService.createEnvelope(formTypeId)
    val f: ServiceResponse[Form] = for {
      _ <- t
      envelopeId <- e
      form <- FormService.insertEmpty(userId, formTypeId, envelopeId, FormId(userId, formTypeId))
    } yield form

    f.fold(
      error => {
        Logger.error(s"""\n\n====> ${error.toString}\n\n""")
        error.toResult
      },
      response => Ok(Json.toJson(response))
    )
  }

  def all() = Action.async { implicit request =>
    Future.successful(NotImplemented)
  }

  def allById(formTypeId: FormTypeId) = Action.async { implicit request =>
    FormService.allById(formTypeId).fold(
      error => error.toResult,
      response => {
        val links = response.map(formLink)
        Ok(Json.toJson(links))
      }
    )
  }

  def getByIdAndVersion(formTypeId: FormTypeId, version: Version) = Action.async { implicit request =>
    FormService.getByIdAndVersion(formTypeId, version).fold(
      error => error.toResult,
      response => {
        val links = response.map(formLink)
        Ok(Json.toJson(links))
      }
    )
  }

  def get(formId: FormId) = Action.async { implicit request =>
    FormService.get(formId).fold(
      error => NotFound,
      response => {
        Logger.debug(Json.prettyPrint(Json.toJson(response)))
        Ok(Json.toJson(response))
      }
    )
  }

  def testEndPoint(formId: FormId) = Action.async { implicit request =>
    FormService.get(formId).fold(
      error => error.toResult,
      response => Ok(Json.toJson(response))
    )
  }

  def update(formId: FormId, tolerant: Option[Boolean]) = Action.async(parse.json[FormData]) { implicit request =>
    val operation = tolerant match {
      case Some(true) => UpdateTolerantOperation
      case _ => UpdateOperation
    }
    FormService.updateFormData(formId, request.body).fold(
      er => BadRequest(er.jsonResponse),
      success => success.toResult
    )
  }

  def submission(formId: FormId) = Action.async { implicit request =>
    SubmissionService.submission(formId).fold(
      error => error.toResult,
      response => Ok(response)
    )
  }

  def delete(formId: FormId): Action[AnyContent] = Action.async { implicit request =>
    FormService.delete(formId).fold(
      errors => {
        Logger.warn(s"${formId.value} failed to be deleted due to ${errors.toResult}")
        errors.toResult
      },
      success => {
        success.toResult
      }
    )
  }

  def submissionStatus(formTypeId: FormTypeId, formId: FormId) = Action.async { implicit request =>
    Future.successful(NotImplemented)
  }

  private def formLink(form: Form)(implicit request: RequestHeader) = {
    val Form(formId, formData, envelopeId) = form
    routes.FormController.get(formId).absoluteURL()
  }
}
