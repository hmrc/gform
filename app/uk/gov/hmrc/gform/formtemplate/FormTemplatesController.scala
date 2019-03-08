/*
 * Copyright 2019 HM Revenue & Customs
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
import play.api.Logger
import play.api.mvc.Action
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, FormTemplateRaw, FormTemplateRawId }

import scala.concurrent.ExecutionContext

class FormTemplatesController(formTemplateService: FormTemplateService)(implicit ex: ExecutionContext)
    extends BaseController {

  def upsert() = Action.async(parse.json[FormTemplateRaw]) { implicit request =>
    //TODO authorisation (we don't want allow everyone to call this action
    val templateRaw: FormTemplateRaw = request.body
    Logger.info(s"Upserting template: ${templateRaw._id.value}, ${loggingHelpers.cleanHeaders(request.headers)}")

    new FormTemplatesControllerRequestHandler(formTemplateService.verifyAndSave, formTemplateService.save).futureInterpreter
      .handleRequest(templateRaw)
  }

  def get(id: FormTemplateId) = Action.async { implicit request =>
    Logger.info(s"Get template, template id: '${id.value}', ${loggingHelpers.cleanHeaders(request.headers)}")
    formTemplateService
      .get(id)
      .asOkJson
  }

  def getRaw(id: FormTemplateRawId) = Action.async { implicit request =>
    Logger.info(s"Get raw template, template id: '${id.value}', ${loggingHelpers.cleanHeaders(request.headers)}")
    formTemplateService
      .get(id)
      .asOkJson
  }

  def remove(formTemplateId: FormTemplateId) = Action.async { implicit request =>
    Logger.info(
      s"Deleting template, template id: '${formTemplateId.value}', ${loggingHelpers.cleanHeaders(request.headers)}")
    //TODO authorisation (we don't want allow everyone to call this action

    val result = for {
      r <- formTemplateService.delete(formTemplateId)
    } yield r

    result.fold(_.asBadRequest, _ => NoContent)
  }

  def all() = Action.async { implicit request =>
    Logger.info(s"Get all templates, ${loggingHelpers.cleanHeaders(request.headers)}")

    formTemplateService
      .list()
      .map(_.map(_._id))
      .asOkJson
  }
}
