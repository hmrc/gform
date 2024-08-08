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
import java.nio.charset.StandardCharsets
import org.slf4j.{ Logger, LoggerFactory }
import org.typelevel.ci.CIString
import play.api.libs.json.{ JsResult, JsValue, Json }
import play.api.mvc.{ Action, AnyContent, ControllerComponents, Result }

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.crypto.{ Crypted, Decrypter, Encrypter, PlainText }
import uk.gov.hmrc.gform.config.{ AppConfig, FileInfoConfig }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.formtemplate.FormTemplateService
import uk.gov.hmrc.gform.objectstore.ObjectStoreAlgebra
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId, Form, FormField, FormIdData, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AllowedFileTypes, FileUpload, FormComponentId, IsFileUpload }

import java.net.URL

class UpscanController(
  appConfig: AppConfig,
  queryParameterCrypto: Encrypter with Decrypter,
  formService: FormAlgebra[Future],
  upscanService: UpscanService,
  formTemplateService: FormTemplateService,
  controllerComponents: ControllerComponents,
  objectStoreAlgebra: ObjectStoreAlgebra[Future]
)(implicit ec: ExecutionContext)
    extends BaseController(controllerComponents) {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private def setTransferred(
    form: Form,
    formComponentId: FormComponentId,
    fileId: FileId,
    fileName: String
  ): Form = form.copy(
    formData = form.formData.copy(
      fields = form.formData.fields :+ FormField(formComponentId, fileName)
    ),
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
            val maybeFileUpload: Option[FileUpload] = formTemplate.formComponents {
              case fc @ IsFileUpload(fu) if fc.id === formComponentId.reduceToTemplateFieldId => fu
            }.headOption

            val allowedFileTypes: AllowedFileTypes =
              maybeFileUpload.flatMap(_.allowedFileTypes).getOrElse(formTemplate.allowedFileTypes)

            val fileSizeLimit = maybeFileUpload
              .flatMap(_.fileSizeLimit)
              .getOrElse(formTemplate.fileSizeLimit.getOrElse(appConfig.formMaxAttachmentSizeMB))

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
                for {
                  form <- formService.get(formIdData)
                  mapping = form.componentIdToFileId.mapping
                              .filter(_._1.reduceToTemplateFieldId === formComponentId.reduceToTemplateFieldId)
                  inverseMapping = mapping.map { case (k, v) => (v, k) }
                  fileId = inverseMapping
                             .get(FileId(formComponentId.value))
                             .flatMap(_ =>
                               mapping.keys.toList
                                 .diff(mapping.values.toList.map(fileId => FormComponentId(fileId.value)))
                                 .headOption
                                 .map(fc => FileId(fc.value))
                             )
                             .getOrElse(FileId(formComponentId.value))
                  fileName = fileId.value + "_" + upscanCallbackSuccess.uploadDetails.fileName
                  validatedFileName: Validated[UpscanValidationFailure, Unit] = validateFileName(fileName)
                  _ <- validatedFileName match {
                         case Invalid(upscanValidationFailure) =>
                           upscanService.reject(
                             upscanCallbackSuccess.reference,
                             UpscanFileStatus.Failed,
                             ConfirmationFailure.GformValidationFailure(upscanValidationFailure)
                           )
                         case Valid(_) =>
                           for {
                             _ <- objectStoreAlgebra.uploadFromUrl(
                                    new URL(upscanCallbackSuccess.downloadUrl),
                                    envelopeId,
                                    fileId,
                                    ContentType(upscanCallbackSuccess.uploadDetails.fileMimeType),
                                    fileName
                                  )
                             formUpd = setTransferred(form, formComponentId, fileId, fileName)
                             _ <- formService.updateUserData(formIdData, toUserData(formUpd))
                             _ <- upscanService.confirm(upscanCallbackSuccess)
                           } yield NoContent
                       }
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

    val validateMatchingExtension: Validated[UpscanValidationFailure, Unit] =
      validateFileExtensionMatchesContentType(uploadDetails.fileName, ContentType(uploadDetails.fileMimeType))

    Valid(uploadDetails)
      .ensure(UpscanValidationFailure.EntityTooSmall)(_.size =!= 0)
      .ensure(UpscanValidationFailure.EntityTooLarge)(_ => validateFileSize(fileSizeLimit, uploadDetails.size))
      .ensure(
        UpscanValidationFailure.InvalidFileType(
          "fileName: " + uploadDetails.fileName + " - " + fileNameCheckResult + ", fileMimeType: " + uploadDetails.fileMimeType + " - " + fileMimeTypeResult,
          ContentType(uploadDetails.fileMimeType)
        )
      )(_ => fileNameCheckResult && fileMimeTypeResult)
      .andThen(_ => validateMatchingExtension)
      .map(_ => ())
  }

  private def validateFileName(fileName: String): Validated[UpscanValidationFailure, Unit] =
    Valid(fileName)
      .ensure(UpscanValidationFailure.FileNameTooLong)(fileNameLength(_) < 255)
      .map(_ => ())

  private def fileNameLength(fileName: String): Int =
    java.net.URLEncoder.encode(fileName, StandardCharsets.UTF_8.toString).length

  private def getFileExtension(fileName: String): Option[String] =
    fileName.split("\\.").tail.lastOption

  private def validateFileExtension(fileName: String): Boolean =
    getFileExtension(fileName).fold(false) { v =>
      !appConfig.restrictedFileExtensions.map(_.value).contains(CIString(v))
    }

  private def validateFileExtensionMatchesContentType(
    fileName: String,
    contentType: ContentType
  ): Validated[UpscanValidationFailure, Unit] =
    getFileExtension(fileName).flatMap { fileExtension =>
      FileInfoConfig.fileExtension(contentType).map { expectedExtensions =>
        if (expectedExtensions.toList.contains(fileExtension.toLowerCase)) {
          Valid(())
        } else Invalid(UpscanValidationFailure.InvalidFileExtension(expectedExtensions.head))
      }
    } match {
      case Some(v) => v
      case None    => Valid(()) // If extension is missing or unknown, we cannot handle it here
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
