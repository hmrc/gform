/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.data

import org.mongodb.scala.model.{ IndexModel, IndexOptions }
import org.mongodb.scala.model.Indexes.ascending
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.formtemplate.FormTemplateModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

import scala.concurrent.{ ExecutionContext, Future }

class DataModule(mongoModule: MongoModule, formTemplateModule: FormTemplateModule, configModule: ConfigModule)(implicit
  ex: ExecutionContext
) {

  private val formRepo: Repo[Form] =
    new Repo[Form](
      "forms",
      mongoModule.mongoComponent,
      _._id.value,
      Seq(
        IndexModel(ascending("data.form.formTemplateId"), IndexOptions().name("formTemplateIdIdx").background(true))
      )
    )

  val dataService: DataAlgebra[Future] = new DataService(formRepo, formTemplateModule.formTemplateService)

  val dataController: DataController =
    new DataController(configModule.controllerComponents, dataService)

  val foptDataService: DataAlgebra[FOpt] = (formTemplateId: FormTemplateId) =>
    fromFutureA(dataService.getSavedFormCount(formTemplateId))
}
