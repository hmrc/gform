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

package uk.gov.hmrc.gform.form

import play.api.Logger
import play.api.http.HttpEntity
import play.api.libs.json.JsValue
import play.api.mvc._
import uk.gov.hmrc.gform.auditing._
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.formtemplate.FormTemplateService
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, FormId, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, UserId }
import uk.gov.hmrc.http.{ BadRequestException, NotFoundException }

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

class FormController(
  config: AppConfig,
  formTemplateService: FormTemplateService,
  fileUpload: FileUploadAlgebra[Future],
  formService: FormAlgebra[Future])(implicit ex: ExecutionContext)
    extends BaseController {

  def newForm(
    userId: UserId,
    formTemplateId: FormTemplateId,
    accessCode: Option[AccessCode]
  ): Action[AnyContent] = Action.async { implicit request =>
    Logger.info(s"new form, userId: '${userId.value}', templateId: '${formTemplateId.value}', accessCode: '${accessCode
      .map(_.value)}', ${loggingHelpers.cleanHeaders(request.headers)}")
    //TODO authentication
    //TODO user should be obtained from secure action
    //TODO authorisation
    //TODO Prevent creating new form when there exist one. Ask user to explicitly delete it
    //TODO: remove userId from argument list (it should be available after authenticating)

    formService.create(userId, formTemplateId, accessCode, config.formExpiryDays.toLong, Seq.empty).asOkJson
  }

  def get(formId: FormId): Action[AnyContent] = Action.async { implicit request =>
    Logger.info(s"getting form, formId: '${formId.value}', ${loggingHelpers.cleanHeaders(request.headers)}")

    //TODO authentication
    //TODO authorisation

    formService
      .get(formId)
      .asOkJson
      .recover {
        case _: NotFoundException => Result(header = ResponseHeader(NOT_FOUND), body = HttpEntity.NoEntity)
      }
  }

  def updateFormData(formId: FormId): Action[UserData] = Action.async(parse.json[UserData]) { implicit request =>
    //TODO: check form status. If after submission don't call this function
    //TODO authentication
    //TODO authorisation
    //TODO do we need to split form data into sections and update only part of the form data related to section? It will

    for {
      _ <- formService.updateUserData(formId, request.body)
    } yield NoContent
  }

  def delete(formId: FormId): Action[AnyContent] = Action.async { implicit request =>
    Logger.info(s"deleting form: '${formId.value}, ${loggingHelpers.cleanHeaders(request.headers)}'")
    formService.delete(formId).asNoContent
  }

  def deleteFile(formId: FormId, fileId: FileId): Action[AnyContent] = Action.async { implicit request =>
    Logger.info(
      s"deleting file, formId: '${formId.value}', fileId: ${fileId.value}, ${loggingHelpers.cleanHeaders(request.headers)} ")
    val result = for {
      form <- formService.get(formId)
      _    <- fileUpload.deleteFile(form.envelopeId, fileId)
    } yield ()
    result.asNoContent
  }

  //TODO discuss with Daniel about naming, purpose of it and if we can make it part of a form
  def saveKeyStore(formId: FormId): Action[Map[String, JsValue]] = Action.async(parse.json[Map[String, JsValue]]) {
    implicit request =>
      formService
        .saveKeyStore(formId, request.body)
        .asNoContent
  }

  //TODO discuss with Daniel about naming, purpose of it and if we can make it part of a form
  def getKeyStore(formId: FormId): Action[AnyContent] = Action.async { implicit request =>
    formService.getKeyStore(formId).asOkJson
  }

  def enrolmentCallBack(formId: FormId): Action[AnyContent] = Action { implicit request =>
    Logger.info(s"Form ID: $formId. Payload: ${request.body.toString}")

    Results.Ok
  }
}
