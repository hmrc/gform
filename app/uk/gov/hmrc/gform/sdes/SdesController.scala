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
import uk.gov.hmrc.gform.objectstore.ObjectStoreAlgebra
import uk.gov.hmrc.gform.sharedmodel.sdes.{ CorrelationId, SdesSubmissionData }

import scala.concurrent.{ ExecutionContext, Future }

class SdesController(
  cc: ControllerComponents,
  sdesAlgebra: SdesAlgebra[Future],
  objectStoreAlgebra: ObjectStoreAlgebra[Future]
)(implicit
  ex: ExecutionContext
) extends BaseController(cc) {

  def search(page: Int, pageSize: Int, processed: Option[Boolean]) = Action.async { _ =>
    sdesAlgebra
      .search(page, pageSize, processed)
      .map(pageData => Ok(Json.toJson(pageData)))
  }

  def find(correlationId: CorrelationId) = Action.async { _ =>
    sdesAlgebra.findSdesSubmission(correlationId).flatMap {
      case Some(s) => Future.successful(Ok(Json.toJson(SdesSubmissionData.fromSdesSubmission(s))))
      case None    => Future.failed(new RuntimeException(s"Correlation id [$correlationId] not found in mongo collection"))
    }
  }

  def notifySDES(correlationId: CorrelationId) = Action.async { implicit request =>
    for {
      sdesSubmission <- sdesAlgebra.findSdesSubmission(correlationId)
      result <- sdesSubmission match {
                  case Some(submission) =>
                    for {
                      objSummary <- objectStoreAlgebra.zipFiles(submission.envelopeId)
                      res        <- sdesAlgebra.notifySDES(submission, objSummary)
                    } yield res
                  case None =>
                    Future.failed(
                      new RuntimeException(s"Correlation id [$correlationId] not found in mongo collection")
                    )
                }
    } yield {
      val status = result.status
      if (status >= 200 && status < 300) {
        Ok
      } else {
        BadRequest(result.body)
      }
    }
  }

  def remove(correlationId: CorrelationId) = Action.async { _ =>
    sdesAlgebra.deleteSdesSubmission(correlationId).map { _ =>
      NoContent
    }
  }
}
