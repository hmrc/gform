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

package uk.gov.hmrc.gform.sdes.workitem

import play.api.libs.json.{ Format, Json }
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.sdes.{ SdesDestination, SdesWorkItem, SdesWorkItemData }
import uk.gov.hmrc.mongo.workitem.{ ProcessingStatus, WorkItem, WorkItemFields }

import scala.concurrent.{ ExecutionContext, Future }

class DestinationWorkItemController(
  cc: ControllerComponents,
  destinationWorkItemAlgebra: DestinationWorkItemAlgebra[Future]
)(implicit
  ex: ExecutionContext
) extends BaseController(cc) {

  def search(
    page: Int,
    pageSize: Int,
    sdesDestination: SdesDestination,
    formTemplateId: Option[FormTemplateId],
    status: Option[ProcessingStatus]
  ) = Action.async { _ =>
    destinationWorkItemAlgebra
      .search(page, pageSize, sdesDestination, formTemplateId, status)
      .map(pageData => Ok(Json.toJson(pageData)))
  }

  def enqueue(id: String, sdesDestination: SdesDestination) = Action.async { _ =>
    destinationWorkItemAlgebra.enqueue(id, sdesDestination).map { _ =>
      Ok
    }
  }

  def get(id: String, sdesDestination: SdesDestination) = Action.async { _ =>
    destinationWorkItemAlgebra.find(id, sdesDestination).flatMap {
      case Some(w) => Future.successful(Ok(Json.toJson(SdesWorkItemData.fromWorkItem(w))))
      case None    => Future.failed(new RuntimeException(s"Object id [$id] not found in mongo collection"))
    }
  }

  def delete(id: String, sdesDestination: SdesDestination) = Action.async { _ =>
    destinationWorkItemAlgebra.delete(id, sdesDestination).map { _ =>
      NoContent
    }
  }

  implicit val workItemFormat: Format[WorkItem[SdesWorkItem]] =
    WorkItem.formatForFields[SdesWorkItem](WorkItemFields.default)
  def getByEnvelopeId(envelopeId: EnvelopeId, sdesDestination: SdesDestination) = Action.async { _ =>
    destinationWorkItemAlgebra.findByEnvelopeId(envelopeId, sdesDestination).map {
      case Nil =>
        Ok(
          s"Not found. There are no data in mongo db collection '$sdesDestination' for envelopeId: ${envelopeId.value}."
        )
      case w :: Nil => Ok(Json.toJson(w))
      case ws       => Ok(Json.toJson(ws))
    }
  }
}
