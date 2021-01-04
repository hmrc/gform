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

package uk.gov.hmrc.gform.formtemplate

import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateId, FormTemplateRaw }

import scala.concurrent.ExecutionContext

class FormTemplateModule(controllerComponents: ControllerComponents, mongoModule: MongoModule)(
  implicit ex: ExecutionContext) {

  private val formTemplateRepo: Repo[FormTemplate] =
    new Repo[FormTemplate]("formTemplate", mongoModule.mongo, _._id.value)
  private val formTemplateRawRepo: Repo[FormTemplateRaw] =
    new Repo[FormTemplateRaw]("formTemplateRaw", mongoModule.mongo, _._id.value)
  val formTemplateService: FormTemplateService = new FormTemplateService(formTemplateRepo, formTemplateRawRepo)
  val formTemplatesController: FormTemplatesController =
    new FormTemplatesController(controllerComponents, formTemplateService)

  val fOptFormTemplateAlgebra: FormTemplateAlgebra[FOpt] = new FormTemplateAlgebra[FOpt] {
    override def get(id: FormTemplateId): FOpt[FormTemplate] = fromFutureA(formTemplateService.get(id))
  }
}
