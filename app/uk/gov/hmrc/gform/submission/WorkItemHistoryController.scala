/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.submission

import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

import scala.concurrent.{ ExecutionContext, Future }

class WorkItemHistoryController(
  controllerComponents: ControllerComponents,
  workItemHistoryService: WorkItemHistoryAlgebra[Future]
)(implicit
  ex: ExecutionContext
) extends BaseController(controllerComponents) {

  def search(page: Int, pageSize: Int, envelopeId: Option[EnvelopeId], formTemplateId: Option[FormTemplateId]) =
    Action.async { _ =>
      workItemHistoryService
        .search(page, pageSize, envelopeId, formTemplateId)
        .map(pageData => Ok(play.api.libs.json.Json.toJson(pageData)))
    }
}
