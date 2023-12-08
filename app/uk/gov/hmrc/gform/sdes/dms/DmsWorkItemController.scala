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

package uk.gov.hmrc.gform.sdes.dms

import play.api.libs.json.Json
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.sdes.{ SdesWorkItem, SdesWorkItemData }
import uk.gov.hmrc.mongo.workitem.{ ProcessingStatus, WorkItem, WorkItemFields }

import scala.concurrent.{ ExecutionContext, Future }

class DmsWorkItemController(
  cc: ControllerComponents,
  dmsWorkItemAlgebra: DmsWorkItemAlgebra[Future]
)(implicit
  ex: ExecutionContext
) extends BaseController(cc) {

  def search(
    page: Int,
    pageSize: Int,
    formTemplateId: Option[FormTemplateId],
    status: Option[ProcessingStatus]
  ) = Action.async { _ =>
    dmsWorkItemAlgebra
      .search(page, pageSize, formTemplateId, status)
      .map(pageData => Ok(Json.toJson(pageData)))
  }

  def enqueue(id: String) = Action.async { _ =>
    dmsWorkItemAlgebra.enqueue(id).map { _ =>
      Ok
    }
  }

  def get(id: String) = Action.async { _ =>
    dmsWorkItemAlgebra.find(id).flatMap {
      case Some(w) => Future.successful(Ok(Json.toJson(SdesWorkItemData.fromWorkItem(w))))
      case None    => Future.failed(new RuntimeException(s"Object id [$id] not found in mongo collection"))
    }
  }

  def delete(id: String) = Action.async { _ =>
    dmsWorkItemAlgebra.delete(id).map { _ =>
      NoContent
    }
  }

  implicit val workItemFormat = WorkItem.formatForFields[SdesWorkItem](WorkItemFields.default)
  def getByEnvelopeId(envelopeId: EnvelopeId) = Action.async { _ =>
    dmsWorkItemAlgebra.findByEnvelopeId(envelopeId).map {
      case Nil =>
        Ok(
          s"Not found. There are no data in mongo db collection 'dmsWorkItem' for envelopeId: ${envelopeId.value}."
        )
      case w :: Nil => Ok(Json.toJson(w))
      case ws       => Ok(Json.toJson(ws))
    }
  }
}
