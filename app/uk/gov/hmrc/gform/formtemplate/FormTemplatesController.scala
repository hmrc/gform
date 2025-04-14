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

package uk.gov.hmrc.gform.formtemplate

import cats.implicits._
import org.slf4j.LoggerFactory
import play.api.libs.json.{ JsObject, Json }
import play.api.mvc.{ Action, AnyContent, ControllerComponents, Results }
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.formredirect.FormRedirectService
import uk.gov.hmrc.gform.formtemplate.FormTemplatePIIRefsHelper.PIIDetailsResponse
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateBehavior, FormTemplateContext, FormTemplateId, FormTemplateRaw, FormTemplateRawId }
import uk.gov.hmrc.gform.shutter.ShutterService
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.translation.TextExtractor

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.notificationbanner.NotificationService
import uk.gov.hmrc.gform.sharedmodel.LatestFormTemplateNotFoundException

class FormTemplatesController(
  controllerComponents: ControllerComponents,
  formTemplateService: FormTemplateService,
  formRedirectService: FormRedirectService,
  shutterService: ShutterService,
  notificationService: NotificationService,
  handler: RequestHandlerAlg[FOpt]
)(implicit
  ex: ExecutionContext
) extends BaseController(controllerComponents) {

  private val logger = LoggerFactory.getLogger(getClass)

  val inputStream = getClass.getClassLoader.getResourceAsStream("formTemplateSchema.json")
  val schemaStream = scala.io.Source.fromInputStream(inputStream).mkString

  def upsert() = Action.async(parse.tolerantText) { implicit request =>
    val templateString: String = request.body
    JsonSchemaValidator.checkSchema(templateString, schemaStream, JsonSchemaErrorParser.parseErrorMessages) match {
      case Left(error) => error.asBadRequest.pure[Future]
      case Right(()) =>
        val jsValue: JsObject =
          Json
            .parse(templateString)
            .as[JsObject] // This parsing should succeed since schema validation detected no errors
        val formTemplateRaw = FormTemplateRaw(jsValue)
        doUpsert(formTemplateRaw)
    }
  }

  // No Schema validation
  def upsertFast() = Action.async(parse.json[FormTemplateRaw]) { implicit request =>
    val formTemplateRaw: FormTemplateRaw = request.body
    addFormTemplateIdToMdc(FormTemplateId(formTemplateRaw._id.value))
    logger.info(s"FormTemplatesController.upsert: ${loggingHelpers.cleanHeaders(request.headers)}")
    doUpsert(formTemplateRaw)
  }

  def validateHtml() = Action.async(parse.tolerantText) { implicit request =>
    val templateString: String = request.body
    val translatableRows: List[(String, String)] = TextExtractor.generateTranslatableRows(templateString)
    val maybeInvalidHtmlError = HtmlValidator.validate(translatableRows)

    maybeInvalidHtmlError match {
      case Some(invalidHtmlError) => BadRequest(invalidHtmlError.asJson).pure[Future]
      case None                   => Ok.pure[Future]
    }

  }

  private def doUpsert(formTemplateRaw: FormTemplateRaw) =
    handler.handleRequest(formTemplateRaw).fold(_.asBadRequest, _ => Results.NoContent)

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
    val formTemplateContext =
      for {
        formTemplate <- findLatestFormTemplate(id)
        redirects    <- formRedirectService.find(formTemplate._id)
      } yield FormTemplateContext(formTemplate, redirects.map(_.redirect))
    formTemplateContext.asOkJson
  }

  def getBehavior(id: FormTemplateId): Action[AnyContent] = formTemplateAction("getBehavior", id) { _ =>
    val formTemplateBehavior =
      for {
        shutter            <- shutterService.find(id)
        notificationBanner <- notificationService.find(id)
      } yield FormTemplateBehavior(shutter, notificationBanner)
    formTemplateBehavior.asOkJson
  }

  private def findFormTemplate(id: FormTemplateId): Future[FormTemplate] =
    formTemplateService
      .find(id)
      .flatMap {
        case Some(ft) => Future.successful(ft)
        case None     => Future.failed(new NoSuchElementException(s"'$id' not found"))
      }

  def getLatest(id: FormTemplateId) = formTemplateAction("getLatest", id) { _ =>
    findLatestFormTemplate(id).asOkJson
  }

  private def findLatestFormTemplate(id: FormTemplateId): Future[FormTemplate] =
    findLatestFormTemplateId(id)
      .flatMap(id =>
        formTemplateService
          .find(id)
          .flatMap {
            case Some(ft) => Future.successful(ft)
            case None     => Future.failed(LatestFormTemplateNotFoundException(s"Latest form template of '$id' not found"))
          }
      )

  private def findLatestFormTemplateId(id: FormTemplateId): Future[FormTemplateId] =
    formRedirectService.find(id) flatMap {
      case Some(fr) => findLatestFormTemplateId(fr.redirect)
      case None     => Future.successful(id)
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

  def getRedirects(page: Int, pageSize: Int) = Action.async { _ =>
    formRedirectService.findAllPaginated(page, pageSize).asOkJson
  }

  def getHandlebarsTemplateIds(formTemplateId: FormTemplateId) = Action.async { implicit request =>
    logger.info(s"FormTemplatesController.getAllHandlebarsIds, ${loggingHelpers.cleanHeaders(request.headers)}")
    formTemplateService.getFormTemplateHandlebars(formTemplateId).map(_.map(_.value)).asOkJson
  }
}
