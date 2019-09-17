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
import play.api.libs.json.Json
import play.api.mvc._
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.auditing._
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.fileupload.FileUploadAlgebra
import uk.gov.hmrc.gform.formtemplate.FormTemplateService
import uk.gov.hmrc.gform.sharedmodel.form.FormIdData
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, AffinityGroupUtil }
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, FormId, FormStatus, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.http.NotFoundException

import scala.concurrent.{ ExecutionContext, Future }

class FormController(
  config: AppConfig,
  formTemplateService: FormTemplateService,
  fileUpload: FileUploadAlgebra[Future],
  formService: FormAlgebra[Future])(implicit ex: ExecutionContext)
    extends BaseController {

  def newForm(
    userId: UserId,
    formTemplateId: FormTemplateId,
    affinityGroup: Option[AffinityGroup]
  ): Action[AnyContent] = Action.async { implicit request =>
    Logger.info(
      s"new form, userId: '${userId.value}', templateId: '${formTemplateId.value}', affinityGroup: '${AffinityGroupUtil
        .affinityGroupNameO(affinityGroup)}', ${loggingHelpers.cleanHeaders(request.headers)}")

    formService.create(userId, formTemplateId, affinityGroup, config.formExpiryDays.toLong, Seq.empty).asOkJson
  }

  def getPlain(userId: UserId, formTemplateId: FormTemplateId): Action[AnyContent] =
    getByForIdData(FormIdData.Plain(userId, formTemplateId))
  def get(userId: UserId, formTemplateId: FormTemplateId, accessCode: AccessCode): Action[AnyContent] =
    getByForIdData(FormIdData.WithAccessCode(userId, formTemplateId, accessCode))

  private def getByForIdData(formIdData: FormIdData): Action[AnyContent] = Action.async { implicit request =>
    Logger.info(
      s"getting form, formId: '${formIdData.toFormId.value}', ${loggingHelpers.cleanHeaders(request.headers)}")
    formService
      .get(formIdData)
      .asOkJson
      .recover {
        case _: NotFoundException => Result(header = ResponseHeader(NOT_FOUND), body = HttpEntity.NoEntity)
      }
  }

  def updateFormDataPlain(userId: UserId, formTemplateId: FormTemplateId): Action[UserData] =
    updateFormDataByForIdData(FormIdData.Plain(userId, formTemplateId))
  def updateFormData(userId: UserId, formTemplateId: FormTemplateId, accessCode: AccessCode): Action[UserData] =
    updateFormDataByForIdData(FormIdData.WithAccessCode(userId, formTemplateId, accessCode))

  private def updateFormDataByForIdData(formIdData: FormIdData): Action[UserData] =
    Action.async(parse.json[UserData]) { implicit request =>
      for {
        _ <- formService.updateUserData(formIdData, request.body)
      } yield NoContent
    }

  def delete(formId: FormId): Action[AnyContent] = Action.async { implicit request =>
    Logger.info(s"deleting form: '${formId.value}, ${loggingHelpers.cleanHeaders(request.headers)}'")
    formService.delete(formId).asNoContent
  }

  def deleteFile(
    userId: UserId,
    formTemplateId: FormTemplateId,
    accessCode: AccessCode,
    fileId: FileId): Action[AnyContent] = Action.async { implicit request =>
    val formIdData = FormIdData.WithAccessCode(userId, formTemplateId, accessCode)
    Logger.info(s"deleting file, formId: '${formIdData.toFormId.value}', fileId: ${fileId.value}, ${loggingHelpers
      .cleanHeaders(request.headers)} ")
    val result = for {
      form <- formService.get(formIdData)
      _    <- fileUpload.deleteFile(form.envelopeId, fileId)
    } yield ()
    result.asNoContent
  }

  def enrolmentCallBack(formId: FormId): Action[AnyContent] = Action { implicit request =>
    Logger.info(s"Form ID: $formId. Payload: ${request.body.toString}")

    Results.Ok
  }

  def getAll(userId: UserId, formTemplateId: FormTemplateId): Action[AnyContent] = Action.async { implicit request =>
    formService.getAll(userId, formTemplateId).map(Json.toJson(_)).map(Ok(_))
  }
}
