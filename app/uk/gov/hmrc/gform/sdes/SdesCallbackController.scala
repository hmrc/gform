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

import cats.syntax.eq._
import org.slf4j.LoggerFactory
import play.api.mvc.{ Action, ControllerComponents }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.objectstore.ObjectStoreAlgebra
import uk.gov.hmrc.gform.sharedmodel.sdes.NotificationStatus.FileProcessed
import uk.gov.hmrc.gform.sharedmodel.sdes.{ CallBackNotification, CorrelationId }

import java.time.Instant
import scala.concurrent.{ ExecutionContext, Future }

class SdesCallbackController(
  cc: ControllerComponents,
  sdesAlgebra: SdesAlgebra[Future],
  objectStoreAlgebra: ObjectStoreAlgebra[Future]
)(implicit ex: ExecutionContext)
    extends BaseController(cc) {
  private val logger = LoggerFactory.getLogger(getClass)

  def callback: Action[CallBackNotification] = Action.async(parse.json[CallBackNotification]) { implicit request =>
    val CallBackNotification(responseStatus, fileName, correlationID, responseFailureReason) = request.body
    logger.info(
      s"Received SDES callback for fileName: $fileName, correlationId : $correlationID, status : $responseStatus and failedReason: ${responseFailureReason
        .getOrElse("")} "
    )

    for {
      maybeSdesSubmission <- sdesAlgebra.findSdesSubmission(CorrelationId(correlationID))
      _ <- maybeSdesSubmission match {
             case Some(sdesSubmission) =>
               val updatedSdesSubmission = sdesSubmission.copy(
                 isProcessed = responseStatus === FileProcessed,
                 status = responseStatus,
                 confirmedAt = Some(Instant.now),
                 failureReason = responseFailureReason
               )
               for {
                 _ <- sdesAlgebra.saveSdesSubmission(updatedSdesSubmission)
                 _ <- if (responseStatus === FileProcessed) objectStoreAlgebra.deleteZipFile(sdesSubmission.envelopeId)
                      else Future.unit
               } yield ()
             case None =>
               Future.failed(new RuntimeException(s"Correlation id [$correlationID] not found in mongo collection"))
           }
    } yield Ok
  }
}
