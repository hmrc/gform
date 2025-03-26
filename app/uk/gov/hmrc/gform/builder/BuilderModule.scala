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

package uk.gov.hmrc.gform.builder

import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.formtemplate.FormTemplateService
import uk.gov.hmrc.gform.formtemplatemetadata.FormTemplateMetadataService
import uk.gov.hmrc.gform.history.HistoryModule

import scala.concurrent.ExecutionContext

class BuilderModule(
  controllerComponents: ControllerComponents,
  formTemplateService: FormTemplateService,
  historyModule: HistoryModule,
  metadataService: FormTemplateMetadataService
)(implicit ex: ExecutionContext) {

  val builderController: BuilderController =
    new BuilderController(controllerComponents, formTemplateService, historyModule.historyService, metadataService)

}
