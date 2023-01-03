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

package uk.gov.hmrc.gform.sdes

import play.api.libs.json.Json
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.objectstore.ObjectStoreAlgebra
import uk.gov.hmrc.gform.sharedmodel.sdes.CorrelationId

import scala.concurrent.{ ExecutionContext, Future }

class SdesController(
  cc: ControllerComponents,
  sdesAlgebra: SdesAlgebra[Future],
  objectStoreAlgebra: ObjectStoreAlgebra[Future]
)(implicit
  ex: ExecutionContext
) extends BaseController(cc) {

  def search(processed: Boolean, page: Int, pageSize: Int) = Action.async { _ =>
    sdesAlgebra
      .search(processed, page, pageSize)
      .map(pageData => Ok(Json.toJson(pageData)))
  }

  def notifySDES(correlationId: CorrelationId) = Action.async { implicit request =>
    for {
      sdesSubmission <- sdesAlgebra.findSdesSubmission(correlationId)
      _ <- sdesSubmission match {
             case Some(submission) =>
               for {
                 objSummary <- objectStoreAlgebra.zipFiles(submission.envelopeId)
                 _          <- sdesAlgebra.notifySDES(submission, objSummary)
               } yield ()
             case None =>
               Future.failed(new RuntimeException(s"Correlation id [$correlationId] not found in mongo collection"))
           }
    } yield NoContent
  }
}
