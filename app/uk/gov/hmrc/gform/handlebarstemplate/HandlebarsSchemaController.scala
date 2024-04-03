/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform.handlebarstemplate

import org.slf4j.LoggerFactory
import play.api.libs.json.Json
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.formtemplate.{ FormTemplateService, RequestHandlerAlg }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateRaw, FormTemplateRawId }
import uk.gov.hmrc.gform.sharedmodel.{ HandlebarsSchema, HandlebarsSchemaId }

import scala.concurrent.{ ExecutionContext, Future }

class HandlebarsSchemaController(
  controllerComponents: ControllerComponents,
  handlebarsSchemaAlgebra: HandlebarsSchemaAlgebra[Future],
  handler: RequestHandlerAlg[FOpt],
  formTemplateService: FormTemplateService
)(implicit
  ex: ExecutionContext
) extends BaseController(controllerComponents) {
  private val logger = LoggerFactory.getLogger(getClass)

  def upsert(handlebarsSchemaId: HandlebarsSchemaId) = Action.async(parse.json) { implicit request =>
    val handlebarsSchema = HandlebarsSchema(handlebarsSchemaId, request.body)

    val formTemplateRawId = FormTemplateRawId(handlebarsSchemaId.value)
    for {
      formTemplateRawOpt <- formTemplateService.get(formTemplateRawId).map(Some(_)).recover {
                              // do not reload template if this is the first upsert
                              case _: NoSuchElementException => None
                            }
      _ <- handlebarsSchemaAlgebra.save(handlebarsSchema)
      _ <- formTemplateRawOpt match {
             case Some(formTemplateRaw) => doTemplateUpsert(formTemplateRaw)
             case None                  => Future.successful(())
           }
    } yield NoContent
  }

  private def doTemplateUpsert(formTemplateRaw: FormTemplateRaw) =
    handler.handleRequest(formTemplateRaw).fold(_.asBadRequest, _ => NoContent)

  def get(handlebarsSchemaId: HandlebarsSchemaId) = Action.async { _ =>
    handlebarsSchemaAlgebra
      .get(handlebarsSchemaId)
      .map(res => Ok(Json.toJson(res)))
  }

  def delete(handlebarsSchemaId: HandlebarsSchemaId) = Action.async { _ =>
    handlebarsSchemaAlgebra
      .delete(handlebarsSchemaId)
      .map(res => Ok(Json.toJson(res)))
  }

  def allIds = Action.async { implicit request =>
    logger.info(s"HandlebarsTemplateController.all, ${loggingHelpers.cleanHeaders(request.headers)}")
    handlebarsSchemaAlgebra.getAllIds.asOkJson
  }
}
