/*
 * Copyright 2019 HM Revenue & Customs
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

import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateId }

import scala.concurrent.ExecutionContext

class FormTemplateModule(mongoModule: MongoModule)(implicit ex: ExecutionContext) {

  val formTemplateRepo: FormTemplateRepo = new FormTemplateRepo(mongoModule.mongo)
  val formTemplateRawRepo: FormTemplateRawRepo = new FormTemplateRawRepo(mongoModule.mongo)
  val superFormTemplateRepo: SuperFormTemplateRepo = new SuperFormTemplateRepo(mongoModule.mongo)
  val formTemplateService: FormTemplateService = new FormTemplateService(formTemplateRepo, formTemplateRawRepo)
  val superFormTemplateService: SuperFormTemplateService = new SuperFormTemplateService(superFormTemplateRepo)
  val formTemplatesController: FormTemplatesController =
    new FormTemplatesController(formTemplateService, superFormTemplateService)

  val fOptFormTemplateAlgebra: FormTemplateAlgebra[FOpt] = new FormTemplateAlgebra[FOpt] {
    override def get(id: FormTemplateId): FOpt[FormTemplate] = fromFutureA(formTemplateService.get(id))
  }

}
