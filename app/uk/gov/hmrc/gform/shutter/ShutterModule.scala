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

package uk.gov.hmrc.gform.shutter

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.mongo.MongoModule

class ShutterModule(
  mongoModule: MongoModule,
  configModule: ConfigModule
)(implicit
  ex: ExecutionContext
) {

  private val shutterRepository: ShutterRepository = new ShutterRepository(mongoModule)
  private val shutterFormTemplateRepository: ShutterFormTemplateRepository =
    new ShutterFormTemplateRepository(mongoModule)

  private val shutterService: ShutterService =
    new ShutterService(
      shutterRepository
    )

  private val shutterFormTemplateService: ShutterFormTemplateService =
    new ShutterFormTemplateService(
      shutterFormTemplateRepository
    )

  val shutterController: ShutterController =
    new ShutterController(
      shutterService,
      shutterFormTemplateService,
      configModule.controllerComponents
    )

  val shutterFormTemplateController: ShutterFormTemplateController =
    new ShutterFormTemplateController(
      shutterFormTemplateService,
      configModule.controllerComponents
    )
}
