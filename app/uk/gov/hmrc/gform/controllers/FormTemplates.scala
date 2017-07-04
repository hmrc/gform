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
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.mvc.Action

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.InvalidState
import uk.gov.hmrc.gform.repositories.{ AbstractRepo, FormTemplateRepository, SchemaRepository }
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import uk.gov.hmrc.play.microservice.controller.BaseController
import uk.gov.hmrc.gform.core.SchemaValidator
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.typeclasses.{ FindOne, Update }

object FormTemplates {
  def saveTemplate(
    formTypeId: FormTypeId,
    version: Version,
    formTemplate: FormTemplate
  )(
    implicit
    ec: ExecutionContext,
    findOne: FindOne[Schema],
    update: Update[FormTemplate]
  ): ServiceResponse[DbOperationResult] = {
    val schemaId = formTemplate.schemaId.getOrElse("http://hmrc.gov.uk/jsonschema/gf-formtemplate#")

    val sectionsList = formTemplate.sections

    val exprs = sectionsList.flatMap(_.fields.map(_.`type`))

    // format: OFF
    for {
      _          <- fromOptA          (Section.validateChoiceHelpText(sectionsList).toEither)
      _          <- fromOptA          (Section.validateUniqueFields(sectionsList).toEither)
      _          <- fromOptA          (ComponentType.validate(exprs, formTemplate).toEither)
      schema     <- fromFutureOptionA (findOne(Json.obj("id" -> schemaId)))(InvalidState(s"SchemaId $schemaId not found"))
      jsonSchema <- fromOptA          (SchemaValidator.conform(schema))
      _          <- fromOptA          (jsonSchema.conform(formTemplate).toEither)
      res        <- fromFutureOptA    (update(Json.obj("formTypeId" -> formTypeId.value, "version" -> version.value), formTemplate))
    } yield res
    // format: ON
  }
}

class FormTemplates()(
    implicit
    schemaRepository: AbstractRepo[Schema],
    formTemplateRepository: AbstractRepo[FormTemplate]
) extends BaseController {

  def all() = Action.async { implicit request =>
    formTemplateRepository.find().map(formTemplates => Ok(Json.toJson(formTemplates)))
  }

  val formTypeIdAndVersionReads: Reads[(FormTypeId, Version)] =
    ((__ \ "formTypeId").read[FormTypeId] and
      (__ \ "version").read[Version]).apply((_, _))

  def save() = Action.async(JsonExtractor[(FormTypeId, Version), FormTemplate](formTypeIdAndVersionReads)) { implicit request =>

    val ((formTypeId, version), templateJson) = request.body

    FormTemplates.saveTemplate(formTypeId, version, templateJson).fold(
      error => error.toResult,
      response => response.toResult
    )
  }

  def allById(formTypeId: FormTypeId) = Action.async { implicit request =>
    formTemplateRepository.find("formTypeId" -> formTypeId).map(formTemplates => Ok(Json.toJson(formTemplates)))
  }

  def get(formTypeId: FormTypeId, version: Version) = Action.async { implicit request =>
    val FindOne = implicitly[FindOne[FormTemplate]]
    FindOne(
      Json.obj(
        "formTypeId" -> formTypeId.value,
        "version" -> version.value
      )
    ).map {
        case Some(formTemplate) => Ok(Json.toJson(formTemplate))
        case None => NoContent
      }
  }

  def delete(formTypeId: FormTypeId, version: Version) = Action.async { implicit request =>
    formTemplateRepository.remove(
      "formTypeId" -> formTypeId.value,
      "version" -> version.value
    ).map { res =>
        res.errmsg match {
          case Some(err) => InternalServerError(Json.toJson(err))
          case None => NoContent
        }
      }
  }
}
