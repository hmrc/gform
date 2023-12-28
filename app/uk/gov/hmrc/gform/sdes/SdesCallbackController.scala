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
import uk.gov.hmrc.gform.sharedmodel.sdes.{ CallBackNotification, CorrelationId, SdesDestination, SdesHistory }

import java.time.Instant
import scala.concurrent.{ ExecutionContext, Future }

class SdesCallbackController(
  cc: ControllerComponents,
  sdesAlgebra: SdesAlgebra[Future],
  objectStoreAlgebra: ObjectStoreAlgebra[Future],
  sdesHistoryAlgebra: SdesHistoryAlgebra[Future]
)(implicit ex: ExecutionContext)
    extends BaseController(cc) {
  private val logger = LoggerFactory.getLogger(getClass)
  def callback: Action[CallBackNotification] = Action.async(parse.json[CallBackNotification]) { implicit request =>
    val CallBackNotification(responseStatus, fileName, correlationID, responseFailureReason) = request.body
    logger.info(
      s"SDES: Received callback for fileName: $fileName, correlationId: $correlationID, status: $responseStatus and failedReason: $responseFailureReason"
    )
    val correlationId = CorrelationId(correlationID)

    for {
      maybeSdesSubmission <- sdesAlgebra.findSdesSubmission(correlationId)
      _ <- maybeSdesSubmission match {
             case Some(sdesSubmission) =>
               val envelopeId = sdesSubmission.envelopeId
               logger.info(
                 s"Received callback for envelopeId: $envelopeId, destination: ${sdesSubmission.destination.getOrElse("dms")}"
               )

               val sdesHistory = SdesHistory.create(
                 envelopeId,
                 correlationId,
                 responseStatus,
                 fileName,
                 responseFailureReason,
                 None
               )

               val updatedSdesSubmission = sdesSubmission.copy(
                 isProcessed = responseStatus === FileProcessed,
                 status = responseStatus,
                 confirmedAt = Some(Instant.now),
                 failureReason = responseFailureReason
               )
               for {
                 _ <- sdesHistoryAlgebra.save(sdesHistory)
                 _ <- sdesAlgebra.saveSdesSubmission(updatedSdesSubmission)
                 _ <- if (responseStatus === FileProcessed) {
                        val sdesDestination = sdesSubmission.sdesDestination
                        val paths = sdesDestination.objectStorePaths(envelopeId)
                        sdesDestination match {
                          case SdesDestination.DataStore | SdesDestination.DataStoreLegacy |
                              SdesDestination.HmrcIlluminate =>
                            objectStoreAlgebra.deleteFile(paths.ephemeral, fileName)
                          case SdesDestination.Dms => objectStoreAlgebra.deleteZipFile(envelopeId, paths)
                        }
                      } else Future.unit
               } yield ()
             case None =>
               Future.failed(new RuntimeException(s"Correlation id [$correlationID] not found in mongo collection"))
           }
    } yield Ok
  }
}
