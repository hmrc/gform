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

package uk.gov.hmrc.gform.fileupload

import akka.util.ByteString
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.time.TimeModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext

class FileUploadModule(
  configModule: ConfigModule,
  wSHttpModule: WSHttpModule,
  timeModule: TimeModule,
  akkaModule: AkkaModule)(implicit ex: ExecutionContext) {

  val fileUploadConnector: FileUploadConnector =
    new FileUploadConnector(config, wSHttpModule.auditableWSHttp, timeModule.timeProvider)

  val fileUploadFrontendConnector: FileUploadFrontendConnector =
    new FileUploadFrontendConnector(config, wSHttpModule.auditableWSHttp)(ex, akkaModule.actorSystem.scheduler)

  val fileUploadService: FileUploadService =
    new FileUploadService(fileUploadConnector, fileUploadFrontendConnector, timeModule.timeProvider)

  val foptFileDownloadService = new FileDownloadAlgebra[FOpt] {
    override def getEnvelope(envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): FOpt[Envelope] =
      fromFutureA(fileUploadService.getEnvelope(envelopeId))

    override def getFileBytes(envelopeId: EnvelopeId, fileId: FileId)(implicit hc: HeaderCarrier): FOpt[ByteString] =
      fromFutureA(fileUploadService.getFileBytes(envelopeId, fileId))
  }

  private lazy val config: FUConfig = FUConfig(
    fileUploadBaseUrl,
    fileUploadFrontendBaseUrl,
    ac.formExpiryDays,
    s"${ac.formMaxAttachmentTotalSizeMB}MB", //heuristic to compute max size
    s"${ac.formMaxAttachmentSizeMB}MB",
    ac.formMaxAttachments,
    configModule.appConfig.contentTypes
  )

  //TODO: provide separate one here
  private lazy val ac = configModule.appConfig

  private lazy val fileUploadBaseUrl = {
    val baseUrl = configModule.serviceConfig.baseUrl("file-upload")
    val pathPrefix = configModule.serviceConfig.getConfString("file-upload.path-prefix", "")
    baseUrl + pathPrefix
  }

  private lazy val fileUploadFrontendBaseUrl = configModule.serviceConfig.baseUrl("file-upload-frontend")
}
