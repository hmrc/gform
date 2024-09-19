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
import play.api.mvc.{ ControllerComponents, Results }
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.formredirect.FormRedirectService
import uk.gov.hmrc.gform.formtemplate.FormTemplatePIIRefsHelper.PIIDetailsResponse
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateContext, FormTemplateId, FormTemplateRaw, FormTemplateRawId }
import uk.gov.hmrc.gform.shutter.ShutterService
import uk.gov.hmrc.gform.core.FOpt

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.notificationbanner.NotificationService
import uk.gov.hmrc.gform.sharedmodel.LatestFormTemplateNotFoundException
import uk.gov.hmrc.gform.testonly.{ Snapshot, SnapshotMongoCache }

class FormTemplatesController(
  controllerComponents: ControllerComponents,
  formTemplateService: FormTemplateService,
  formRedirectService: FormRedirectService,
  shutterService: ShutterService,
  notificationService: NotificationService,
  handler: RequestHandlerAlg[FOpt],
  snapshotMongoCache: SnapshotMongoCache,
  isProd: Boolean
)(implicit
  ex: ExecutionContext
) extends BaseController(controllerComponents) {
  private val logger = LoggerFactory.getLogger(getClass)
  private val formTemplateIdSchemaError = "Error at <#/_id>: Property _id expected value [a-zA-Z0-9-]]"

  def upsert() = Action.async(parse.tolerantText) { implicit request =>
    val templateString: String = request.body
    val inputStream = getClass.getClassLoader.getResourceAsStream("formTemplateSchema.json")
    val schemaStream = scala.io.Source.fromInputStream(inputStream).mkString

    JsonSchemaValidator.checkSchema(templateString, schemaStream, JsonSchemaErrorParser.parseErrorMessages) match {
      case Left(error) => error.asBadRequest.pure[Future]
      case Right(()) =>
        val jsValue: JsObject =
          Json
            .parse(templateString)
            .as[JsObject] // This parsing should succeed since schema validation detected no errors
        val formTemplateRaw = FormTemplateRaw(jsValue)
        for {
          maybeSnapshot <- maybeSnapshot(formTemplateRaw.lowerCaseId._id)
          res <- if (isProd && formTemplateRaw._id.value.contains("_")) {
                   Future.failed(
                     new IllegalArgumentException(formTemplateIdSchemaError)
                   )
                 } else doUpsert(formTemplateRaw)
          _ <- maybeSnapshot match {
                 case Some(snapshot) => upsertSnapshot(snapshot, formTemplateRaw.lowerCaseId)
                 case _              => ().pure[Future]
               }
        } yield res
    }
  }

  // No Schema validation
  def upsertFast() = Action.async(parse.json[FormTemplateRaw]) { implicit request =>
    val formTemplateRaw: FormTemplateRaw = request.body
    addFormTemplateIdToMdc(FormTemplateId(formTemplateRaw._id.value))
    logger.info(s"FormTemplatesController.upsert: ${loggingHelpers.cleanHeaders(request.headers)}")
    doUpsert(formTemplateRaw)
  }

  private def doUpsert(formTemplateRaw: FormTemplateRaw) =
    handler.handleRequest(formTemplateRaw).fold(_.asBadRequest, _ => Results.NoContent)

  private def maybeSnapshot(formTemplateRawId: FormTemplateRawId): Future[Option[Snapshot]] =
    if (!isProd && formTemplateRawId.value.contains("_")) {
      for {
        maybeSnapshot <- snapshotMongoCache.findBySnapshotTemplateId(FormTemplateId(formTemplateRawId.value))
        res <- maybeSnapshot match {
                 case Some(snapshot) => Some(snapshot).pure[Future]
                 case _ =>
                   if (formTemplateRawId.value.contains("_")) {
                     Future.failed(
                       new IllegalArgumentException(formTemplateIdSchemaError)
                     )
                   } else None.pure[Future]
               }
      } yield res
    } else None.pure[Future]

  private def upsertSnapshot(snapshot: Snapshot, formTemplateRaw: FormTemplateRaw) =
    snapshotMongoCache
      .upsert(snapshot.copy(originalTemplate = formTemplateRaw))
      .as(logger.info(s"snapshotMongoCache.upsert(${formTemplateRaw._id.value})"))

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
        formTemplate            <- findLatestFormTemplate(id)
        redirects               <- formRedirectService.find(formTemplate._id)
        mayBeShutter            <- shutterService.find(formTemplate._id)
        maybeNotificationBanner <- notificationService.find(formTemplate._id)
      } yield FormTemplateContext(formTemplate, redirects.map(_.redirect), mayBeShutter, maybeNotificationBanner)
    formTemplateContext.asOkJson
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
