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
import uk.gov.hmrc.gform.formtemplate.FormTemplateService
import uk.gov.hmrc.gform.formtemplate.RequestHandlerAlg
import uk.gov.hmrc.gform.sharedmodel.HandlebarsTemplate
import uk.gov.hmrc.gform.sharedmodel.HandlebarsTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateRaw
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateRawId

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class HandlebarsTemplateController(
  controllerComponents: ControllerComponents,
  handlebarsTemplateAlgebra: HandlebarsTemplateAlgebra[Future],
  handler: RequestHandlerAlg[FOpt],
  formTemplateService: FormTemplateService
)(implicit
  ex: ExecutionContext
) extends BaseController(controllerComponents) {
  private val logger = LoggerFactory.getLogger(getClass)

  def upsert(handlebarsTemplateId: HandlebarsTemplateId) = Action.async(parse.text) { implicit request =>
    val handleBarsTemplate = HandlebarsTemplate(handlebarsTemplateId, request.body)

    val formTemplateIdStr = handlebarsTemplateId.value.replaceAll("-(\\w+)$", "")
    val formTemplateRawId = FormTemplateRawId(formTemplateIdStr)
    for {
      formTemplateRaw <- formTemplateService.get(formTemplateRawId)
      _               <- doTemplateUpsert(formTemplateRaw)
      _               <- handlebarsTemplateAlgebra.save(handleBarsTemplate)
    } yield NoContent
  }

  private def doTemplateUpsert(formTemplateRaw: FormTemplateRaw) =
    handler.handleRequest(formTemplateRaw).fold(_.asBadRequest, _ => NoContent)

  def getRaw(handlebarsTemplateId: HandlebarsTemplateId) = Action.async { _ =>
    handlebarsTemplateAlgebra
      .get(handlebarsTemplateId)
      .map(handlebarsPayload => Ok(handlebarsPayload.map(_.payload).getOrElse("")).as("text/plain"))
  }

  def get(handlebarsTemplateId: HandlebarsTemplateId) = Action.async { _ =>
    handlebarsTemplateAlgebra
      .get(handlebarsTemplateId)
      .map(handlebarsPayload => Ok(Json.toJson(handlebarsPayload.map(_.payload))))
  }

  def delete(handlebarsTemplateId: HandlebarsTemplateId) = Action.async { _ =>
    handlebarsTemplateAlgebra
      .delete(handlebarsTemplateId)
      .map(res => Ok(Json.toJson(res)))
  }

  def all = Action.async { implicit request =>
    logger.info(s"HandlebarsTemplateController.all, ${loggingHelpers.cleanHeaders(request.headers)}")
    handlebarsTemplateAlgebra.getAll.asOkJson
  }
}
