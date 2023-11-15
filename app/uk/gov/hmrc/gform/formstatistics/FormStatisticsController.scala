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

import play.api.libs.json.Json
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

import scala.concurrent.{ ExecutionContext, Future }

class FormStatisticsController(
  controllerComponents: ControllerComponents,
  formStatisticsService: FormStatisticsAlgebra[Future]
)(implicit
  ex: ExecutionContext
) extends BaseController(controllerComponents) {

  def getAllSavedVersions() = Action.async { _ =>
    formStatisticsService
      .getAllSavedVersions()
      .map(allSavedVersion => Ok(Json.toJson(allSavedVersion)))
  }

  def getFormCount(formTemplateId: FormTemplateId) = Action.async { _ =>
    formStatisticsService
      .getSavedFormCount(formTemplateId)
      .map(savedForm => Ok(Json.toJson(savedForm)))
  }

  def getFormCountDetails(formTemplateId: FormTemplateId) = Action.async { _ =>
    formStatisticsService
      .getSavedFormDetails(formTemplateId)
      .map(savedFormDetail => Ok(Json.toJson(savedFormDetail)))
  }

  def getSignedFormDetails() = Action.async { _ =>
    formStatisticsService
      .getSignedFormDetails()
      .map { signedFormDetails =>
        Ok(Json.toJson(signedFormDetails))
      }
  }
}
