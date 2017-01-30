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

package uk.gov.hmrc.bforms.controllers

import cats.instances.future._
import java.util.UUID
import play.api.libs.json.Json
import play.api.mvc.{ Action, Request, RequestHeader }
import scala.concurrent.Future
import uk.gov.hmrc.bforms.model.{ Form, FormData, FormId, FormTypeId }
import uk.gov.hmrc.bforms.repositories.{ FormRepository, FormTemplateRepository }
import uk.gov.hmrc.bforms.services.{ FormService, MongoOperation, SaveOperation, UpdateOperation }
import uk.gov.hmrc.play.microservice.controller.BaseController
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

class Forms()(
    implicit
    formRepo: FormRepository,
    formTemplateRepo: FormTemplateRepository
) extends BaseController {
  def all() = Action.async { implicit request =>
    Future.successful(NotImplemented)
  }

  def save() = Action.async(parse.json[FormData]) { implicit request =>
    val _id = FormId(UUID.randomUUID().toString())
    saveOrUpdate(_id, SaveOperation)
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

  def update(formId: FormId) = Action.async(parse.json[FormData]) { implicit request =>
    saveOrUpdate(formId, UpdateOperation)
  }

  def delete(formTypeId: FormTypeId, version: String, formId: FormId) = Action.async { implicit request =>
    Future.successful(NotImplemented)
  }

  def submission(formTypeId: FormTypeId, formId: FormId) = Action.async { implicit request =>
    Future.successful(NotImplemented)
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
    routes.Forms.get(formData.formTypeId, formData.version, formId).absoluteURL()
  }
}
