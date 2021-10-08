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

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.crypto.CryptoWithKeysFromConfig
import uk.gov.hmrc.gform.config.{ AppConfig, ConfigModule }
import uk.gov.hmrc.gform.fileupload.FileUploadFrontendAlgebra
import uk.gov.hmrc.gform.form.FormService
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule

class UpscanModule(
  formService: FormService[Future],
  wSHttpModule: WSHttpModule,
  configModule: ConfigModule,
  queryParameterCrypto: CryptoWithKeysFromConfig,
  fileUploadFrontendAlgebra: FileUploadFrontendAlgebra[Future],
  appConfig: AppConfig,
  mongoModule: MongoModule
)(implicit
  ec: ExecutionContext
) {

  private val upscanConnector = new UpscanConnector(wSHttpModule.auditableWSHttp)

  private val upscanRepository = new UpscanRepository(appConfig, mongoModule.mongoComponent)

  val upscanService: UpscanService = new UpscanService(
    upscanConnector,
    upscanRepository
  )

  val upscanController: UpscanController =
    new UpscanController(
      queryParameterCrypto,
      formService,
      upscanService,
      fileUploadFrontendAlgebra,
      configModule.controllerComponents
    )

}
