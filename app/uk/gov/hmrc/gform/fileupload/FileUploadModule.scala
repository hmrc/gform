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

package uk.gov.hmrc.gform.fileupload

import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.envelope.EnvelopeModule
import uk.gov.hmrc.gform.objectstore.{ FUConfig, ObjectStoreModule }
import uk.gov.hmrc.gform.sdes.SdesModule
import uk.gov.hmrc.gform.time.TimeModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule

import scala.concurrent.ExecutionContext

class FileUploadModule(
  configModule: ConfigModule,
  wSHttpModule: WSHttpModule,
  timeModule: TimeModule,
  akkaModule: AkkaModule,
  envelopeModule: EnvelopeModule,
  objectStoreModule: ObjectStoreModule,
  sdesModule: SdesModule
)(implicit ex: ExecutionContext) {

  val fileUploadConnector: FileUploadConnector =
    new FileUploadConnector(
      config,
      wSHttpModule.httpClient,
      envelopeModule.envelopeService
    )

  val fileUploadFrontendConnector: FileUploadFrontendConnector =
    new FileUploadFrontendConnector(config, wSHttpModule.httpClient)(ex, akkaModule.actorSystem.scheduler)

  val fileUploadService: FileUploadService =
    new FileUploadService(
      fileUploadConnector,
      fileUploadFrontendConnector,
      timeModule.timeProvider,
      objectStoreModule.objectStoreService,
      sdesModule.destinationWorkItemService
    )

  private lazy val config: FUConfig = FUConfig(
    fileUploadBaseUrl,
    fileUploadFrontendBaseUrl,
    ac.formExpiryDays,
    s"${ac.formMaxAttachmentTotalSizeMB}MB", //heuristic to compute max size
    ac.formMaxAttachmentSizeMB.toString,
    ac.formMaxAttachments
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
