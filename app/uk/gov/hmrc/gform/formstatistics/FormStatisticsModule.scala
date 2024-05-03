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

package uk.gov.hmrc.gform.formstatistics

import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.formtemplate.FormTemplateModule
import uk.gov.hmrc.gform.sharedmodel.form.{ AllSavedVersions, SavedFormDetail, SignedFormDetails, VersionStats }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.mongo.cache.MongoCacheRepository

import scala.concurrent.{ ExecutionContext, Future }

class FormStatisticsModule(
  formsCacheRepository: MongoCacheRepository[String],
  formTemplateModule: FormTemplateModule,
  configModule: ConfigModule
)(implicit
  ex: ExecutionContext
) {
  val formStatisticsService: FormStatisticsAlgebra[Future] =
    new FormStatisticsService(formsCacheRepository, formTemplateModule.formTemplateService)

  val formStatisticsController: FormStatisticsController =
    new FormStatisticsController(configModule.controllerComponents, formStatisticsService)

  val foptFormStatisticsService: FormStatisticsAlgebra[FOpt] = new FormStatisticsAlgebra[FOpt] {
    override def getAllSavedVersions(): FOpt[AllSavedVersions] =
      fromFutureA(formStatisticsService.getAllSavedVersions())

    override def getSavedFormCount(formTemplateId: FormTemplateId): FOpt[Seq[VersionStats]] =
      fromFutureA(formStatisticsService.getSavedFormCount(formTemplateId))

    override def getSavedFormDetails(formTemplateId: FormTemplateId): FOpt[Seq[SavedFormDetail]] =
      fromFutureA(formStatisticsService.getSavedFormDetails(formTemplateId))

    override def getSignedFormDetails(): FOpt[Seq[SignedFormDetails]] =
      fromFutureA(formStatisticsService.getSignedFormDetails())
  }
}
