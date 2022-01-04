/*
 * Copyright 2022 HM Revenue & Customs
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
import play.api.libs.json.Json
import play.api.mvc.{ ControllerComponents, Results }
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.formredirect.FormRedirectService
import uk.gov.hmrc.gform.formtemplate.FormTemplatePIIRefsHelper.PIIDetailsResponse
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateId, FormTemplateRaw, FormTemplateRawId, FormTemplateWithRedirects }

import scala.concurrent.{ ExecutionContext, Future }

class FormTemplatesController(
  controllerComponents: ControllerComponents,
  formTemplateService: FormTemplateService,
  formRedirectService: FormRedirectService
)(implicit
  ex: ExecutionContext
) extends BaseController(controllerComponents) {
  private val logger = LoggerFactory.getLogger(getClass)

  def upsert() = Action.async(parse.json[FormTemplateRaw]) { implicit request =>
    val templateRaw: FormTemplateRaw = request.body
    addFormTemplateIdToMdc(FormTemplateId(templateRaw._id.value))
    logger.info(s"FormTemplatesController.upsert: ${loggingHelpers.cleanHeaders(request.headers)}")

    new FormTemplatesControllerRequestHandler(
      formTemplateService.verifyAndSave,
      formTemplateService.save
    ).futureInterpreter
      .handleRequest(templateRaw)
      .fold(_.asBadRequest, _ => Results.NoContent)
  }

  def getTitlesWithPII(formTemplateRawId: FormTemplateRawId, filters: Option[String], includeJson: Boolean) =
    Action.async { _ =>
      formTemplateService
        .get(formTemplateRawId)
        .map { ftr =>
          val json = Json.prettyPrint(ftr.value)
          PIIDetailsResponse(
            FormTemplatePIIRefsHelper
              .getTitlesWithPII(json, filters.map(_.split(",").toList).getOrElse(List.empty)),
            if (includeJson) Some(json) else None
          )
        }
        .asOkJson
    }

  def get(id: FormTemplateId) = formTemplateAction("get", id) { _ =>
    findFormTemplate(id).asOkJson
  }

  def getWithRedirects(id: FormTemplateId) = formTemplateAction("getWithRedirects", id) { _ =>
    val formTemplateWithRedirects =
      for {
        formTemplate <- findFormTemplate(id)
        redirects    <- formRedirectService.find(formTemplate._id)
      } yield FormTemplateWithRedirects(formTemplate, redirects.map(_.redirect))
    formTemplateWithRedirects.asOkJson
  }

  private def findFormTemplate(id: FormTemplateId): Future[FormTemplate] =
    formTemplateService
      .find(id)
      .flatMap {
        case Some(ft) => Future.successful(ft)
        case None =>
          formRedirectService.find(id) flatMap {
            case Some(fr) => findFormTemplate(fr.redirect)
            case None     => Future.failed(new NoSuchElementException(s"'$id' not found"))
          }
      }

  def getRaw(id: FormTemplateRawId) = formTemplateAction("getRaw", FormTemplateId(id.value)) { _ =>
    formTemplateService
      .get(id)
      .asOkJson
  }

  def getRawSensitive(id: String) = formTemplateAction("getRaw", FormTemplateId(id)) { _ =>
    formTemplateService
      .get(FormTemplateRawId(id))
      .asOkJson
  }

  def remove(formTemplateId: FormTemplateId) = formTemplateAction("remove", formTemplateId) { _ =>
    val result = for {
      r <- formTemplateService.delete(formTemplateId)
    } yield r

    result.fold(_.asBadRequest, deleteResults => Ok(Json.toJson(deleteResults)))
  }

  def removeSensitive(formTemplateId: String) = formTemplateAction("remove", FormTemplateId(formTemplateId)) { _ =>
    val result = for {
      r <- formTemplateService.delete(FormTemplateId(formTemplateId))
    } yield r

    result.fold(_.asBadRequest, deleteResults => Ok(Json.toJson(deleteResults)))
  }

  def all() = Action.async { implicit request =>
    logger.info(s"FormTemplatesController.all, ${loggingHelpers.cleanHeaders(request.headers)}")

    formTemplateService.list().asOkJson
  }
}
