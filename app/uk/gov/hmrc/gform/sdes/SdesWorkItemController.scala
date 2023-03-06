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

package uk.gov.hmrc.gform.sdes

import play.api.libs.json.Json
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesWorkItemData
import uk.gov.hmrc.mongo.workitem.ProcessingStatus

import scala.concurrent.{ ExecutionContext, Future }

class SdesWorkItemController(
  cc: ControllerComponents,
  sdesWorkItemAlgebra: SdesWorkItemAlgebra[Future]
)(implicit
  ex: ExecutionContext
) extends BaseController(cc) {

  def search(
    page: Int,
    pageSize: Int,
    formTemplateId: Option[FormTemplateId],
    status: Option[ProcessingStatus]
  ) = Action.async { _ =>
    sdesWorkItemAlgebra
      .search(page, pageSize, formTemplateId, status)
      .map(pageData => Ok(Json.toJson(pageData)))
  }

  def enqueue(id: String) = Action.async { _ =>
    sdesWorkItemAlgebra.enqueue(id).map { _ =>
      Ok
    }
  }

  def get(id: String) = Action.async { _ =>
    sdesWorkItemAlgebra.find(id).flatMap {
      case Some(w) => Future.successful(Ok(Json.toJson(SdesWorkItemData.fromWorkItem(w))))
      case None    => Future.failed(new RuntimeException(s"Object id [$id] not found in mongo collection"))
    }
  }

  def delete(id: String) = Action.async { _ =>
    sdesWorkItemAlgebra.delete(id).map { _ =>
      NoContent
    }
  }
}
