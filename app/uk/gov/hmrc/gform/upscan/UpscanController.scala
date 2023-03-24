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

package uk.gov.hmrc.gform.upscan

import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import cats.instances.future._
import cats.instances.long._
import cats.syntax.eq._
import cats.syntax.either._
import cats.syntax.applicative._
import org.slf4j.{ Logger, LoggerFactory }
import org.typelevel.ci.CIString
import play.api.libs.json.{ JsResult, JsValue, Json }
import play.api.mvc.{ Action, AnyContent, ControllerComponents, Result }

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.crypto.{ Crypted, Decrypter, Encrypter, PlainText }
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.fileupload.FileUploadFrontendAlgebra
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.formtemplate.FormTemplateService
import uk.gov.hmrc.gform.objectstore.ObjectStoreAlgebra
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId, Form, FormIdData, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AllowedFileTypes, FileUpload, FileUploadProvider, FormComponentId, IsFileUpload }

class UpscanController(
  appConfig: AppConfig,
  queryParameterCrypto: Encrypter with Decrypter,
  formService: FormAlgebra[Future],
  upscanService: UpscanService,
  fileUploadFrontendAlgebra: FileUploadFrontendAlgebra[Future],
  formTemplateService: FormTemplateService,
  controllerComponents: ControllerComponents,
  objectStoreAlgebra: ObjectStoreAlgebra[Future]
)(implicit ec: ExecutionContext)
    extends BaseController(controllerComponents) {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private def setTransferred(
    form: Form,
    formComponentId: FormComponentId,
    fileId: FileId
  ): Form =
    form.copy(
      componentIdToFileId = form.componentIdToFileId + (formComponentId, fileId)
    )

  private def toUserData(form: Form): UserData = UserData(
    form.formData,
    form.status,
    form.visitsIndex,
    form.thirdPartyData,
    form.componentIdToFileId
  )

  def encrypt = Action.async(parse.json) { implicit request =>
    val formIdDataString = Json.stringify(request.body)

    val formIdDataCrypted: Crypted = queryParameterCrypto.encrypt(PlainText(formIdDataString))
    Ok(formIdDataCrypted.value).pure[Future]
  }

  def callback(
    formComponentId: FormComponentId,
    envelopeId: EnvelopeId,
    formIdDataCrypted: Crypted
  ): Action[JsValue] =
    Action.async(parse.json) { implicit request =>
      logger.info(s"Upscan callback - received notification for $formComponentId")

      val upscanCallbackPayload: JsResult[UpscanCallback] = request.body.validate[UpscanCallback]

      val decrypted: PlainText = queryParameterCrypto.decrypt(formIdDataCrypted)

      val formIdDataJsResult: JsResult[FormIdData] = Json.parse(decrypted.value).validate[FormIdData]
      val fileId = FileId(formComponentId.value)

      val callbackResult: Either[Unit, Future[Result]] = for {
        formIdData <-
          formIdDataJsResult.asEither.leftMap(error =>
            logger.error(s"Upscan callback - formIdData failed to parse: $formIdDataJsResult, reported error: $error")
          )
        upscanCallback <-
          upscanCallbackPayload.asEither.leftMap(error =>
            logger.error(s"Upscan callback payload failed to parse: ${request.body}, reported error: $error")
          )

      } yield upscanCallback match {
        case upscanCallbackSuccess: UpscanCallback.Success =>
          formTemplateService.get(formIdData.formTemplateId).flatMap { formTemplate =>
            logger.info(
              s"Upscan callback successful, fcId: $formComponentId, reference: ${upscanCallbackSuccess.reference}, fileMimeType: ${upscanCallbackSuccess.uploadDetails.fileMimeType}, fileName: ${upscanCallbackSuccess.uploadDetails.fileName}, size: ${upscanCallbackSuccess.uploadDetails.size}"
            )
            val allowedFileTypes: AllowedFileTypes = formTemplate.allowedFileTypes
            val fileSizeLimit = formTemplate.fileSizeLimit.getOrElse(appConfig.formMaxAttachments)
            val validated: Validated[UpscanValidationFailure, Unit] =
              validateFile(allowedFileTypes, fileSizeLimit, upscanCallbackSuccess.uploadDetails)

            validated match {
              case Invalid(upscanValidationFailure) =>
                for {
                  _ <- upscanService.reject(
                         upscanCallbackSuccess.reference,
                         UpscanFileStatus.Failed,
                         ConfirmationFailure.GformValidationFailure(upscanValidationFailure)
                       )
                } yield NoContent
              case Valid(_) =>
                val maybeFileUpload: Option[FileUpload] = formTemplate.formComponents {
                  case fc @ IsFileUpload(fileupload) if fc.id === formComponentId => fileupload
                }.headOption
                val compression: Boolean = maybeFileUpload.fold(false) {
                  case FileUpload(FileUploadProvider.Upscan(true)) => true
                  case _                                           => false
                }
                for {
                  file <- upscanService.download(upscanCallbackSuccess.downloadUrl)
                  compressedFile = ImageCompressor.compressIfSupported(file, upscanCallbackSuccess, compression)
                  uploadDetails = upscanCallbackSuccess.uploadDetails
                  _ <- if (formTemplate.isObjectStore) {
                         val fileName = fileId.value + "_" + uploadDetails.fileName
                         objectStoreAlgebra.uploadFile(
                           envelopeId,
                           fileId,
                           fileName,
                           compressedFile,
                           ContentType(uploadDetails.fileMimeType)
                         )
                       } else {
                         fileUploadFrontendAlgebra
                           .uploadFile(envelopeId, fileId, uploadDetails, compressedFile)
                       }
                  form <- formService.get(formIdData)
                  formUpd = setTransferred(form, formComponentId, fileId)
                  _ <- formService.updateUserData(formIdData, toUserData(formUpd))
                  _ <- upscanService.confirm(upscanCallbackSuccess)
                } yield NoContent
            }
          }

        case upscanCallbackFailure: UpscanCallback.Failure =>
          val ref = upscanCallbackFailure.reference.value
          val fileStatus = upscanCallbackFailure.fileStatus
          val failureReason = upscanCallbackFailure.failureDetails.failureReason
          val message = upscanCallbackFailure.failureDetails.message
          logger.info(
            s"Upscan callback failed, fcId: $formComponentId. Reference $ref, status: $fileStatus, failureReason: $failureReason, message: $message"
          )
          for {
            upscanConfirmation <- upscanService.reject(
                                    upscanCallbackFailure.reference,
                                    upscanCallbackFailure.fileStatus,
                                    ConfirmationFailure.UpscanFailure(upscanCallbackFailure.failureDetails)
                                  )
          } yield NoContent
      }

      callbackResult.getOrElse(NoContent.pure[Future])

    }

  private def validateFile(
    allowedFileTypes: AllowedFileTypes,
    fileSizeLimit: Int,
    uploadDetails: UploadDetails
  ): Validated[UpscanValidationFailure, Unit] = {
    val fileNameCheckResult = validateFileExtension(uploadDetails.fileName)
    val fileMimeTypeResult = validateFileType(allowedFileTypes, ContentType(uploadDetails.fileMimeType))
    Valid(uploadDetails)
      .ensure(UpscanValidationFailure.EntityTooSmall)(_.size =!= 0)
      .ensure(UpscanValidationFailure.EntityTooLarge)(_ => validateFileSize(fileSizeLimit, uploadDetails.size))
      .ensure(
        UpscanValidationFailure.InvalidFileType(
          "fileName: " + uploadDetails.fileName + " - " + fileNameCheckResult + ", fileMimeType: " + uploadDetails.fileMimeType + " - " + fileMimeTypeResult,
          ContentType(uploadDetails.fileMimeType)
        )
      )(_ => fileNameCheckResult && fileMimeTypeResult)
      .map(_ => ())
  }

  private def getFileExtension(fileName: String): Option[String] =
    fileName.split("\\.").tail.lastOption

  private def validateFileExtension(fileName: String): Boolean =
    getFileExtension(fileName).fold(false) { v =>
      !appConfig.restrictedFileExtensions.map(_.value).contains(CIString(v))
    }

  private def validateFileType(allowedFileTypes: AllowedFileTypes, contentType: ContentType): Boolean =
    allowedFileTypes.contentTypes.exists(_ === contentType)

  private def validateFileSize(fileSizeLimit: Int, size: Long): Boolean =
    size <= (fileSizeLimit * 1024 * 1024).toLong

  def reference(upscanReference: UpscanReference): Action[AnyContent] =
    Action.async { request =>
      upscanService.reference(upscanReference).map { maybeUpscanConfirmation =>
        maybeUpscanConfirmation
          .fold[Result](NotFound)(upscanConfirmation => Ok(Json.toJson(upscanConfirmation)))
      }
    }
  def deleteReference(upscanReference: UpscanReference): Action[AnyContent] =
    Action.async { request =>
      upscanService.deleteReference(upscanReference).map(_ => NoContent)
    }

}
