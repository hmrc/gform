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

import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import play.api.Logger
import play.api.libs.json.{ Json, OFormat }
import play.api.mvc.{ Action, AnyContent, Request, RequestHeader }
import uk.gov.hmrc.gform.connectors.Save4LaterConnector
import uk.gov.hmrc.gform.controllers.FormController._
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.repositories.{ AbstractRepo, SubmissionRepository }
import uk.gov.hmrc.gform.services.{ FormService, MongoOperation, SaveOperation, SaveTolerantOperation, SubmissionService, UpdateOperation, UpdateTolerantOperation, _ }
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

  def newForm(userId: UserId, formTypeId: FormTypeId, version: Version) = Action.async { implicit request =>

    val template = FormTemplateService.get(formTypeId, version)
    val envelopeId = FileUploadService.createEnvelope(formTypeId)
    val form = envelopeId.flatMap(envelopeId => FormService.insertEmpty(userId, formTypeId, version, envelopeId))
    val response = for {
      t <- template
      e <- envelopeId
      f <- form
    } yield NewFormResponse(f, e, t)
    response.fold(
      error => error.toResult,
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

  def getByUserId(userId: UserId, formTypeId: FormTypeId, version: String) = Action.async { implicit request =>
    FormService.getByUserId(userId, formTypeId, version).fold(
      error => NoContent,
      response => {
        val index = Index(response._id, response.envelopeId)
        Ok(Json.toJson(index))
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

  def get(formTypeId: FormTypeId, version: Version, formId: FormId) = Action.async { implicit request =>
    FormService.get(formTypeId, version, formId).fold(
      error => error.toResult,
      response => Ok(Json.toJson(response.formData))
    )
  }

  def getCache(formTypeId: FormTypeId, version: Version, userId: UserId) = Action.async { implicit request =>
    val formKey = FormKey(userId + formTypeId.value, version.value)
    FormService.get(formKey).fold(
      error => error.toResult,
      response => Ok(Json.toJson(response))
    )
  }

  def update(formId: FormId, tolerant: Option[Boolean]) = Action.async(parse.json[FormData]) { implicit request =>
    val operation = tolerant match {
      case Some(true) => UpdateTolerantOperation
      case _ => UpdateOperation
    }
    FormService.updateFormData(formId, request.body).fold(er => BadRequest(er.jsonResponse), success => success.toResult)
  }

  def submission(formTypeId: FormTypeId, formId: FormId) = Action.async { implicit request =>
    SubmissionService.submission(formTypeId, formId).fold(
      error => error.toResult,
      response => Ok(response)
    )
  }

  def submissionCache(formTypeId: FormTypeId, userId: UserId, version: String) = Action.async { implicit request =>
    SubmissionService.submission(formTypeId, userId, version).fold(
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
    routes.FormController.get(formData.formTypeId, formData.version, formId).absoluteURL()
  }
}

object FormController {

  case class NewFormResponse(
    form: Form,
    envelopeId: EnvelopeId,
    formTemplate: FormTemplate
  )

  object NewFormResponse {
    implicit val format: OFormat[NewFormResponse] = Json.format[NewFormResponse]
  }

}