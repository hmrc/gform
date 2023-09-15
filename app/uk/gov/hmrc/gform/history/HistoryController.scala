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

package uk.gov.hmrc.gform.history

import play.api.libs.json.Json
import play.api.mvc.{ ControllerComponents, Result }
import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateRawId

class HistoryController(controllerComponents: ControllerComponents, historyService: HistoryService)(implicit
  ex: ExecutionContext
) extends BaseController(controllerComponents) {

  def allFormTemplateIds() = Action.async { request =>
    historyService.allFormTemplateIds().map(formTemplateRawIds => Ok(Json.toJson(formTemplateRawIds)))
  }

  def formTemplateHistoryOverview(formTemplateRawId: FormTemplateRawId) = Action.async { request =>
    historyService
      .formTemplateHistoryOverview(formTemplateRawId)
      .map { formTemplateHistories =>
        Ok(Json.toJson(formTemplateHistories))
      }
  }

  def historyFor(historyId: HistoryId) = Action.async { request =>
    historyService
      .historyFor(historyId)
      .map { maybeFormTemplateRaw =>
        maybeFormTemplateRaw.fold[Result](NotFound)(formTemplateRaw => Ok(Json.toJson(formTemplateRaw)))
      }
  }

  def overviewWithFilter() =
    Action.async(parse.json[HistoryFilter]) { request =>
      historyService
        .overviewWithFilter(request.body)
        .map { historyOverviewFull =>
          Ok(Json.toJson(historyOverviewFull))
        }
    }

  def previousHistoryId(
    formTemplateRawId: FormTemplateRawId,
    historyId: HistoryId
  ) =
    Action.async { request =>
      historyService
        .previousHistoryId(formTemplateRawId, historyId)
        .map { maybeHistoryId =>
          maybeHistoryId.fold[Result](NotFound)(historyId => Ok(Json.toJson(historyId)))
        }
    }

  def nextHistoryId(
    formTemplateRawId: FormTemplateRawId,
    historyId: HistoryId
  ) =
    Action.async { request =>
      historyService
        .nextHistoryId(formTemplateRawId, historyId)
        .map { maybeHistoryId =>
          maybeHistoryId.fold[Result](NotFound)(historyId => Ok(Json.toJson(historyId)))
        }
    }
}
