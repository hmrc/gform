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

package uk.gov.hmrc.gform.translation

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.formtemplate.FormTemplateModule
import uk.gov.hmrc.gform.gformfrontend.GformFrontendConnector
import uk.gov.hmrc.gform.history.HistoryModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.translation.audit.TranslationAuditRepository

class TranslationModule(
  formTemplateModule: FormTemplateModule,
  historyModule: HistoryModule,
  configModule: ConfigModule,
  mongoModule: MongoModule,
  gformFrontendConnector: GformFrontendConnector
)(implicit
  ex: ExecutionContext
) {
  val translationAuditRepo: TranslationAuditRepository = new TranslationAuditRepository(
    mongoModule,
    configModule.appConfig
  )

  val translationService: TranslationService = new TranslationService(
    translationAuditRepo
  )

  val translationController: TranslationController =
    new TranslationController(
      formTemplateModule.formTemplateService,
      historyModule.historyService,
      translationService,
      configModule.controllerComponents,
      gformFrontendConnector
    )

}
