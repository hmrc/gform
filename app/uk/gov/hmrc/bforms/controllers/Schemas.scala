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
import scala.concurrent.ExecutionContext
import play.api.mvc.Action
import play.api.libs.json._
import uk.gov.hmrc.bforms.core._
import uk.gov.hmrc.bforms.model.{ DbOperationResult, Schema }
import uk.gov.hmrc.bforms.typeclasses.Update
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import uk.gov.hmrc.play.microservice.controller.BaseController
import uk.gov.hmrc.bforms.repositories.SchemaRepository

object Schemas {
  def saveSchema(
    schemaId: String,
    schema: Schema
  )(
    implicit
    ec: ExecutionContext,
    update: Update[Schema]
  ): ServiceResponse[DbOperationResult] = {
    fromFutureOptA(update(Json.obj("id" -> schemaId), schema))
  }
}

class Schemas()(
    implicit
    schemaRepository: SchemaRepository
) extends BaseController {
  def all = Action.async { implicit request =>
    schemaRepository.find().map(schemas => Ok(Json.toJson(schemas)))
  }

  def save = Action.async(JsonWithKey[Schema]("id")) { implicit request =>
    val (id, schema) = request.body

    Schemas.saveSchema(id, schema).fold(
      error => error.toResult,
      response => response.toResult
    )
  }
}
