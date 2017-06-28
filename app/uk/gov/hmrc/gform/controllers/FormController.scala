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
import java.util.UUID

import play.api.libs.json.{ Json, OFormat }
import play.api.mvc.{ Action, Request, RequestHeader }

import scala.concurrent.Future
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.repositories.{ FormRepository, FormTemplateRepository, SubmissionRepository }
import uk.gov.hmrc.gform.services._
import uk.gov.hmrc.gform.typeclasses.{ FindOne, FusFeUrl, FusUrl, ServiceUrl }
import uk.gov.hmrc.play.microservice.controller.BaseController
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import FormController._
import cats.data.EitherT
import uk.gov.hmrc.gform.core.{ Opt, ServiceResponse }
import uk.gov.hmrc.gform.exceptions.UnexpectedState

class FormController()(
    implicit
    formRepo: FormRepository,
    formTemplateRepo: FormTemplateRepository,
    submissionRepo: SubmissionRepository,
    fusUrl: ServiceUrl[FusUrl],
    fusFeUrl: ServiceUrl[FusFeUrl]
) extends BaseController {

  def newForm(formTypeId: FormTypeId, version: String) = Action.async { implicit request =>

    val template = FormTemplateService.get(formTypeId, version)
    val envelope = FileUploadService.createEnvelope(formTypeId)
    val form = FormService.insertEmpty(formTypeId, version)
    val response = for {
      t <- template
      e <- envelope
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

  def save(tolerant: Option[Boolean]) = Action.async(parse.json[FormData]) { implicit request =>
    val _id = FormId(UUID.randomUUID().toString())
    val operation = tolerant match {
      case Some(true) => SaveTolerantOperation
      case _ => SaveOperation
    }
    saveOrUpdate(_id, operation)
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

  def getByIdAndVersion(formTypeId: FormTypeId, version: String) = Action.async { implicit request =>
    FormService.getByIdAndVersion(formTypeId, version).fold(
      error => error.toResult,
      response => {
        val links = response.map(formLink)
        Ok(Json.toJson(links))
      }
    )
  }

  def get(formTypeId: FormTypeId, version: String, formId: FormId) = Action.async { implicit request =>
    FormService.get(formTypeId, version, formId).fold(
      error => error.toResult,
      response => Ok(Json.toJson(response.formData))
    )
  }

  def update(formId: FormId, tolerant: Option[Boolean]) = Action.async(parse.json[FormData]) { implicit request =>
    val operation = tolerant match {
      case Some(true) => UpdateTolerantOperation
      case _ => UpdateOperation
    }
    saveOrUpdate(formId, operation)
  }

  def delete(formTypeId: FormTypeId, version: String, formId: FormId) = Action.async { implicit request =>
    Future.successful(NotImplemented)
  }

  def submission(formTypeId: FormTypeId, formId: FormId) = Action.async { implicit request =>
    SubmissionService.submission(formTypeId, formId).fold(
      error => error.toResult,
      response => Ok(response)
    )
  }

  def submissionStatus(formTypeId: FormTypeId, formId: FormId) = Action.async { implicit request =>
    Future.successful(NotImplemented)
  }

  private def saveOrUpdate(formId: FormId, mongoOperation: MongoOperation)(implicit request: Request[FormData]) = {
    val formData = request.body
    val form = Form(formId, formData)

    FormService.saveOrUpdate(form, mongoOperation).fold(
      error => error.toResult,
      response => {
        val formRoute = formLink(form)
        Ok(Json.obj("success" -> formRoute))
      }
    )
  }

  private def formLink(form: Form)(implicit request: RequestHeader) = {
    val Form(formId, formData) = form
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