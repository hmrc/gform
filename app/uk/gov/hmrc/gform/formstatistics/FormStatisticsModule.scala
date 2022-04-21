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

package uk.gov.hmrc.gform.formstatistics

import org.mongodb.scala.model.{ IndexModel, IndexOptions }
import org.mongodb.scala.model.Indexes.ascending
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.formtemplate.FormTemplateModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, SavedForm, SavedFormDetail }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

import scala.concurrent.{ ExecutionContext, Future }

class FormStatisticsModule(
  mongoModule: MongoModule,
  formTemplateModule: FormTemplateModule,
  configModule: ConfigModule
)(implicit
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

  val formStatisticsService: FormStatisticsAlgebra[Future] =
    new FormStatisticsService(formRepo, formTemplateModule.formTemplateService)

  val formStatisticsController: FormStatisticsController =
    new FormStatisticsController(configModule.controllerComponents, formStatisticsService)

  val foptFormStatisticsService: FormStatisticsAlgebra[FOpt] = new FormStatisticsAlgebra[FOpt] {
    override def getSavedFormCount(formTemplateId: FormTemplateId): FOpt[SavedForm] =
      fromFutureA(formStatisticsService.getSavedFormCount(formTemplateId))

    override def getSavedFormDetails(formTemplateId: FormTemplateId): FOpt[Seq[SavedFormDetail]] =
      fromFutureA(formStatisticsService.getSavedFormDetails(formTemplateId))
  }
}
