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

package uk.gov.hmrc.gform.upscan

import cats.instances.future._
import cats.syntax.either._
import cats.syntax.applicative._
import cats.data.Validated
import cats.instances.int._
import cats.syntax.eq._
import cats.data.Validated.{ Invalid, Valid }
import org.slf4j.{ Logger, LoggerFactory }
import play.api.libs.json.{ JsResult, JsValue, Json }
import play.api.mvc.{ Action, AnyContent, ControllerComponents, Result }

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.crypto.{ Crypted, CryptoWithKeysFromConfig, PlainText }
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.fileupload.FileUploadFrontendAlgebra
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.sharedmodel.config.{ ContentType, FileExtension }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId, Form, FormIdData, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

class UpscanController(
  appConfig: AppConfig,
  queryParameterCrypto: CryptoWithKeysFromConfig,
  formService: FormAlgebra[Future],
  upscanService: UpscanService,
  fileUploadFrontendAlgebra: FileUploadFrontendAlgebra[Future],
  controllerComponents: ControllerComponents
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

  def callback(formComponentId: FormComponentId, envelopeId: EnvelopeId, formIdDataCrypted: Crypted): Action[JsValue] =
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
          logger.info(
            s"Upscan callback successful, fcId: $formComponentId, reference: ${upscanCallbackSuccess.reference}, fileMimeType: ${upscanCallbackSuccess.uploadDetails.fileMimeType}, fileName: ${upscanCallbackSuccess.uploadDetails.fileName}, size: ${upscanCallbackSuccess.uploadDetails.size}"
          )

          val validated: Validated[FailureDetails, Unit] = validateFile(upscanCallbackSuccess.uploadDetails)

          validated match {
            case Invalid(failureDetails) =>
              for {
                _ <- upscanService.reject(
                       UpscanCallback.Failure(
                         upscanCallbackSuccess.reference,
                         UpscanFileStatus.Failed,
                         failureDetails
                       )
                     )
              } yield NoContent
            case Valid(_) =>
              for {
                file <- upscanService.download(upscanCallbackSuccess.downloadUrl)
                compressedFile = ImageCompressor.compressIfSupported(file, upscanCallbackSuccess)
                _ <-
                  fileUploadFrontendAlgebra
                    .uploadFile(envelopeId, fileId, upscanCallbackSuccess.uploadDetails, compressedFile)
                form <- formService.get(formIdData)
                formUpd = setTransferred(form, formComponentId, fileId)
                _ <- formService.updateUserData(formIdData, toUserData(formUpd))
                _ <- upscanService.confirm(upscanCallbackSuccess)
              } yield NoContent
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
            _ <- upscanService.reject(upscanCallbackFailure)
          } yield NoContent
      }

      callbackResult.getOrElse(NoContent.pure[Future])

    }

  private def validateFile(uploadDetails: UploadDetails): Validated[FailureDetails, Unit] =
    Valid(uploadDetails.fileName)
      .ensure(FailureDetails("REJECTED", "EntityTooSmall"))(_.length =!= 0)
      .ensure(
        FailureDetails("REJECTED", s"MIME type [${getFileExtension(uploadDetails.fileName).getOrElse("unknown")}]")
      )(_ => validateFileExtension(uploadDetails.fileName) && validateFileType(ContentType(uploadDetails.fileName)))
      .ensure(FailureDetails("REJECTED", "EntityTooLarge"))(_ => validateFileSize(uploadDetails.size))
      .map(_ => ())

  private def getFileExtension(fileName: String): Option[FileExtension] =
    fileName.split("\\.").tail.lastOption.map(FileExtension(_))

  private def validateFileExtension(fileName: String): Boolean =
    getFileExtension(fileName).fold(false) { ex =>
      !appConfig.restrictedFileExtensions.exists(_ === ex)
    }

  private def validateFileType(contentType: ContentType): Boolean =
    appConfig.contentTypes.exists(_ === contentType)

  private def validateFileSize(size: Long): Boolean =
    size <= appConfig.formMaxAttachmentSizeMB * 1024 * 1024

  def reference(upscanReference: UpscanReference): Action[AnyContent] =
    Action.async { request =>
      upscanService.reference(upscanReference).map { maybeUpscanConfirmation =>
        maybeUpscanConfirmation.fold[Result](NotFound)(upscanConfirmation => Ok(Json.toJson(upscanConfirmation)))
      }
    }
  def deleteReference(upscanReference: UpscanReference): Action[AnyContent] =
    Action.async { request =>
      upscanService.deleteReference(upscanReference).map(_ => NoContent)
    }
}
