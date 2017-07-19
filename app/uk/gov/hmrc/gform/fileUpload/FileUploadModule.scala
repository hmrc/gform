/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.fileUpload

import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.time.TimeModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule

class FileUploadModule(configModule: ConfigModule, wSHttpModule: WSHttpModule, timeModule: TimeModule) {

  lazy val fileUploadConnector: FileUploadConnector = new FileUploadConnector(config, wSHttpModule.auditableWSHttp, timeModule.localDateTime())
  lazy val fileUploadFrontendConnector: FileUploadFrontendConnector = new FileUploadFrontendConnector(config, wSHttpModule.auditableWSHttp)

  private lazy val config: Config = Config(
    configModule.serviceConfig.baseUrl("file-upload"),
    configModule.serviceConfig.baseUrl("file-upload-frontend"),
    ac.formExpiryDays,
    s"${ac.formMaxAttachments * ac.formMaxAttachmentSizeMB + 10}MB", //heuristic to compute max size
    s"${ac.formMaxAttachmentSizeMB}MB",
    ac.formMaxAttachments
  )

  //TODO: provide separate one here
  private lazy implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
  private lazy val ac = configModule.appConfig
}
