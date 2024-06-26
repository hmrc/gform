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

package uk.gov.hmrc.gform.handlebarstemplate

import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.formtemplate.FormTemplateModule

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class HandlebarsTemplateModule(
  controllerComponents: ControllerComponents,
  handlebarsTemplateService: HandlebarsTemplateAlgebra[Future],
  formTemplateModule: FormTemplateModule,
  handlebarsSchemaService: HandlebarsSchemaAlgebra[Future]
)(implicit
  ex: ExecutionContext
) {
  val handlebarTemplateController: HandlebarsTemplateController =
    new HandlebarsTemplateController(
      controllerComponents,
      handlebarsTemplateService,
      formTemplateModule.handler,
      formTemplateModule.formTemplateService
    )

  val handlebarSchemaController: HandlebarsSchemaController =
    new HandlebarsSchemaController(
      controllerComponents,
      handlebarsSchemaService,
      formTemplateModule.handler,
      formTemplateModule.formTemplateService
    )
}
