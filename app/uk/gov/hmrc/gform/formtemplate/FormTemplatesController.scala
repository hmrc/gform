/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.formtemplate

import cats.implicits._
import org.slf4j.LoggerFactory
import play.api.mvc.{ ControllerComponents, Results }
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, FormTemplateRaw, FormTemplateRawId }

import scala.concurrent.ExecutionContext

class FormTemplatesController(controllerComponents: ControllerComponents, formTemplateService: FormTemplateService)(
  implicit ex: ExecutionContext)
    extends BaseController(controllerComponents) {
  private val logger = LoggerFactory.getLogger(getClass)

  def upsert() = Action.async(parse.json[FormTemplateRaw]) { implicit request =>
    val templateRaw: FormTemplateRaw = request.body
    addFormTemplateIdToMdc(FormTemplateId(templateRaw._id.value))
    logger.info(s"FormTemplatesController.upsert: ${loggingHelpers.cleanHeaders(request.headers)}")

    new FormTemplatesControllerRequestHandler(formTemplateService.verifyAndSave, formTemplateService.save).futureInterpreter
      .handleRequest(templateRaw)
      .fold(_.asBadRequest, _ => Results.NoContent)
  }

  def get(id: FormTemplateId) = formTemplateAction("get", id) { _ =>
    formTemplateService
      .get(id)
      .asOkJson
  }

  def getRaw(id: FormTemplateRawId) = formTemplateAction("getRaw", FormTemplateId(id.value)) { _ =>
    formTemplateService
      .get(id)
      .asOkJson
  }

  def remove(formTemplateId: FormTemplateId) = formTemplateAction("remove", formTemplateId) { _ =>
    val result = for {
      r <- formTemplateService.delete(formTemplateId)
    } yield r

    result.fold(_.asBadRequest, _ => NoContent)
  }

  def all() = Action.async { implicit request =>
    logger.info(s"FormTemplatesController.all, ${loggingHelpers.cleanHeaders(request.headers)}")

    formTemplateService.list().asOkJson
  }
}
