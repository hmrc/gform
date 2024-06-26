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

package uk.gov.hmrc.gform.history

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.mongo.MongoModule

class HistoryModule(configModule: ConfigModule, mongoModule: MongoModule)(implicit ex: ExecutionContext) {
  private val formTemplateHistoryRepo: HistoryRepository =
    new HistoryRepository(mongoModule, configModule.appConfig)

  val historyService: HistoryService = new HistoryService(formTemplateHistoryRepo)

  val historyController: HistoryController =
    new HistoryController(configModule.controllerComponents, historyService)
}
