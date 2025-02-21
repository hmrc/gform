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

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.crypto.{ Decrypter, Encrypter }
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.config.{ AppConfig, ConfigModule }
import uk.gov.hmrc.gform.form.FormService
import uk.gov.hmrc.gform.formtemplate.FormTemplateModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.objectstore.ObjectStoreModule

class UpscanModule(
  formService: FormService[Future],
  configModule: ConfigModule,
  queryParameterCrypto: Encrypter with Decrypter,
  formTemplateModule: FormTemplateModule,
  appConfig: AppConfig,
  mongoModule: MongoModule,
  objectStoreModule: ObjectStoreModule,
  auditingModule: AuditingModule
)(implicit
  ec: ExecutionContext
) {
  private val upscanRepository = new UpscanRepository(appConfig, mongoModule.mongoComponent)

  val upscanService: UpscanService = new UpscanService(
    upscanRepository
  )

  val upscanController: UpscanController =
    new UpscanController(
      appConfig,
      queryParameterCrypto,
      formService,
      upscanService,
      formTemplateModule.formTemplateService,
      configModule.controllerComponents,
      objectStoreModule.objectStoreService,
      auditingModule.auditService
    )

}
