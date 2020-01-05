/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.notifier

import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core.{ FOpt, fOptMonadError }
import uk.gov.service.notify.NotificationClient

import scala.concurrent.ExecutionContext

class NotifierModule(configModule: ConfigModule)(implicit ec: ExecutionContext) {
  private val config = configModule.notifierConfig
  private val client = new NotificationClient(config.apiKey)

  val fOptNotifierService = new NotifierService[FOpt](client)

  val notifierController = new NotifierController(
    configModule.controllerComponents,
    fOptNotifierService,
    configModule.adjudicatorsEmailTemplateId)
}
