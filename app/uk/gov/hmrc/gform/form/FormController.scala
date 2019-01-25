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
import play.api.mvc.{ Action, AnyContent, ResponseHeader, Result }
import uk.gov.hmrc.gform.auditing._
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.core.FormValidator
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.formtemplate.FormTemplateService
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, UserId }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeExpiryDate, FileId, FormId, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.concurrent.Future
import scala.util.Try
import uk.gov.hmrc.http.{ BadRequestException, NotFoundException }

class FormController(
  formTemplateService: FormTemplateService,
  fileUploadService: FileUploadService,
  formService: FormService)
    extends BaseController {

  def newForm(
    userId: UserId,
    formTemplateId: FormTemplateId,
    accessCode: Option[AccessCode]
  ) = Action.async { implicit request =>
    Logger.info(s"new form, userId: '${userId.value}', templateId: '${formTemplateId.value}', accessCode: '${accessCode
      .map(_.value)}', ${loggingHelpers.cleanHeaders(request.headers)}")
    //TODO authentication
    //TODO user should be obtained from secure action
    //TODO authorisation
    //TODO Prevent creating new form when there exist one. Ask user to explicitly delete it
    //TODO: remove userId from argument list (it should be available after authenticating)

    val formId = FormId(userId, formTemplateId, accessCode)
    val envelopeIdF = fileUploadService.createEnvelope(formTemplateId)
    val expiryDate = EnvelopeExpiryDate(envelopeIdF._2)
    val formIdF: Future[FormId] = for {
      envelopeId <- envelopeIdF._1
      _          <- formService.insertEmpty(userId, formTemplateId, envelopeId, formId, expiryDate)

    } yield formId

    formIdF.asOkJson
  }

  def get(formId: FormId) = Action.async { implicit request =>
    Logger.info(s"getting form, formId: '${formId.value}', ${loggingHelpers.cleanHeaders(request.headers)}")

    //TODO authentication
    //TODO authorisation

    formService
      .get(formId)
      .asOkJson
      .recover {
        case e: NotFoundException => {
          Result(header = ResponseHeader(NOT_FOUND), body = HttpEntity.NoEntity)
        }
      }
  }

  def updateFormData(formId: FormId) = Action.async(parse.json[UserData]) { implicit request =>
    //TODO: check form status. If after submission don't call this function
    //TODO authentication
    //TODO authorisation
    //TODO do we need to split form data into sections and update only part of the form data related to section? It will

    for {
      _ <- formService.updateUserData(formId, request.body)
    } yield NoContent
  }

  def validateSection(formId: FormId, sectionNumber: SectionNumber) = Action.async { implicit request =>
    Logger.info(s"Validating sections: '${formId.value}', section number '${sectionNumber.value}', ${loggingHelpers
      .cleanHeaders(request.headers)}")
    //TODO check form status. If after submission don't call this function
    //TODO authentication
    //TODO authorisation
    //TODO wrap result into ValidationResult case class containign status of validation and list of errors

    val result: Future[Either[UnexpectedState, Unit]] = for {
      form         <- formService.get(formId)
      formTemplate <- formTemplateService.get(form.formTemplateId)
      section = getSection(formTemplate, sectionNumber)
    } yield FormValidator.validate(form.formData.fields.toList, section)

    result.map(_.fold(e => e.error, _ => "No errors")).asOkJson
  }

  def delete(formId: FormId): Action[AnyContent] = Action.async { implicit request =>
    Logger.info(s"deleting form: '${formId.value}, ${loggingHelpers.cleanHeaders(request.headers)}'")
    formService.delete(formId).asNoContent
  }

  def deleteFile(formId: FormId, fileId: FileId) = Action.async { implicit request =>
    Logger.info(
      s"deleting file, formId: '${formId.value}', fileId: ${fileId.value}, ${loggingHelpers.cleanHeaders(request.headers)} ")
    val result = for {
      form <- formService.get(formId)
      _    <- fileUploadService.deleteFile(form.envelopeId, fileId)
    } yield ()
    result.asNoContent
  }

  //TODO discuss with Daniel about naming, purpose of it and if we can make it part of a form
  def saveKeyStore(formId: FormId) = Action.async(parse.json[Map[String, JsValue]]) { implicit request =>
    formService
      .saveKeyStore(formId, request.body)
      .asNoContent
  }

  //TODO discuss with Daniel about naming, purpose of it and if we can make it part of a form
  def getKeyStore(formId: FormId) = Action.async { implicit request =>
    formService.getKeyStore(formId).asOkJson
  }

  private def getSection(formTemplate: FormTemplate, sectionNumber: SectionNumber): Section =
    Try(formTemplate.sections(sectionNumber.value))
      .getOrElse(
        throw new BadRequestException(
          s"Wrong sectionNumber: $sectionNumber. There are ${formTemplate.sections.length} sections."))
}
