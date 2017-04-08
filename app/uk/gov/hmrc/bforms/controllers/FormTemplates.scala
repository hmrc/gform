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
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.mvc.Action
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.bforms.core._
import uk.gov.hmrc.bforms.exceptions.InvalidState
import uk.gov.hmrc.bforms.repositories.{ FormTemplateRepository, SchemaRepository }
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import uk.gov.hmrc.play.microservice.controller.BaseController
import uk.gov.hmrc.bforms.core.SchemaValidator
import uk.gov.hmrc.bforms.models.{ ComponentType, Schema, FormTemplate, FormTypeId, DbOperationResult }
import uk.gov.hmrc.bforms.typeclasses.{ FindOne, Update }

object FormTemplates {
  def saveTemplate(
    formTypeId: FormTypeId,
    version: String,
    formTemplate: FormTemplate
  )(
    implicit
    ec: ExecutionContext,
    findOne: FindOne[Schema],
    update: Update[FormTemplate]
  ): ServiceResponse[DbOperationResult] = {
    // Hardcoded for now, should be read from formTemplate itself
    val schemaId = "http://hmrc.gov.uk/jsonschema/bf-formtemplate#"

    val exprs = formTemplate.sections.flatMap(_.fields.map(_.`type`))

    // format: OFF
    for {
      _          <- fromOptA          (ComponentType.validate(exprs, formTemplate).toEither)
      schema     <- fromFutureOptionA (findOne(Json.obj("id" -> schemaId)))(InvalidState(s"SchemaId $schemaId not found"))
      jsonSchema <- fromOptA          (SchemaValidator.conform(schema))
      _          <- fromOptA          (jsonSchema.conform(formTemplate).toEither)
      res        <- fromFutureOptA    (update(Json.obj("formTypeId" -> formTypeId, "version" -> version), formTemplate))
    } yield res
    // format: ON
  }
}

class FormTemplates()(
    implicit
    schemaRepository: SchemaRepository,
    formTemplateRepository: FormTemplateRepository
) extends BaseController {

  def all() = Action.async { implicit request =>
    formTemplateRepository.find().map(formTemplates => Ok(Json.toJson(formTemplates)))
  }

  val formTypeIdAndVersionReads: Reads[(FormTypeId, String)] =
    ((__ \ "formTypeId").read[FormTypeId] and
      (__ \ "version").read[String]).apply((_, _))

  def save() = Action.async(JsonExtractor[(FormTypeId, String), FormTemplate](formTypeIdAndVersionReads)) { implicit request =>

    val ((formTypeId, version), templateJson) = request.body

    FormTemplates.saveTemplate(formTypeId, version, templateJson).fold(
      error => error.toResult,
      response => response.toResult
    )
  }

  def allById(formTypeId: FormTypeId) = Action.async { implicit request =>
    formTemplateRepository.find("formTypeId" -> formTypeId).map(formTemplates => Ok(Json.toJson(formTemplates)))
  }

  def get(formTypeId: FormTypeId, version: String) = Action.async { implicit request =>
    val FindOne = implicitly[FindOne[FormTemplate]]
    FindOne(
      Json.obj(
        "formTypeId" -> formTypeId,
        "version" -> version
      )
    ).map {
        case Some(formTemplate) => Ok(Json.toJson(formTemplate))
        case None => NoContent
      }
  }

  def delete(formTypeId: FormTypeId, version: String) = Action.async { implicit request =>
    Future.successful(NotImplemented)
  }
}
