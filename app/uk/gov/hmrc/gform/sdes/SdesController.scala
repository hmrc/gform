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

import cats.syntax.all._
import play.api.libs.json.Json
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.sdes.{ CorrelationId, SdesFilter, SdesHistoryNotification, SdesHistoryView, SdesSubmissionData }

import scala.concurrent.{ ExecutionContext, Future }
import java.time.{ LocalDateTime, ZoneOffset }

class SdesController(
  cc: ControllerComponents,
  sdesAlgebra: SdesAlgebra[Future],
  sdesRenotifyService: SdesRenotifyService,
  sdesHistoryAlgebra: SdesHistoryAlgebra[Future]
)(implicit
  ex: ExecutionContext
) extends BaseController(cc) {

  def search() = Action.async(parse.json[SdesFilter]) { request =>
    sdesAlgebra
      .search(request.body)
      .map(pageData => Ok(Json.toJson(pageData)))
  }

  def findByEnvelopeId(envelopeId: EnvelopeId) = Action.async { _ =>
    sdesAlgebra.findSdesSubmissionByEnvelopeId(envelopeId).map {
      case Nil =>
        Ok(s"Not found. There are no data in mongo db collection 'sdesSubmission' for envelopeId: ${envelopeId.value}.")
      case s :: Nil => Ok(Json.toJson(s))
      case xs       => Ok(Json.toJson(xs))
    }
  }

  def find(correlationId: CorrelationId) = Action.async { _ =>
    sdesAlgebra.findSdesSubmission(correlationId).flatMap {
      case Some(s) => Future.successful(Ok(Json.toJson(SdesSubmissionData.fromSdesSubmission(s))))
      case None    => Future.failed(new RuntimeException(s"Correlation id [$correlationId] not found in mongo collection"))
    }
  }

  def renotifySDES(correlationId: CorrelationId) = Action.async { implicit request =>
    sdesRenotifyService
      .renotifySDES(correlationId)
      .map { result =>
        val status = result.status
        if (status >= 200 && status < 300) {
          Ok
        } else {
          BadRequest(result.body)
        }
      }
  }

  def updateAsManualConfirmed(correlationId: CorrelationId) = Action.async { _ =>
    sdesAlgebra.updateAsManualConfirmed(correlationId).map { _ =>
      NoContent
    }
  }

  def getSdesSubmissionsDestinations() = Action.async { _ =>
    sdesAlgebra
      .getSdesSubmissionsDestination()
      .map { sdesDestination =>
        Ok(Json.toJson(sdesDestination))
      }
  }

  def sdesMigration(from: String, to: String) = Action.async { _ =>
    (from, to) match {
      case ("DataStore", "DataStoreLegacy") =>
        sdesAlgebra.getSdesSubmissionsDestination().flatMap { stats =>
          stats.find(_.destination === to) match {
            case Some(destination) =>
              BadRequest(s"Invalid migration. 'DataStoreLegacy' destination already exists: $destination").pure[Future]
            case None => runMigration(from, to)
          }
        }
      case ("DataStoreLegacy", "DataStore") =>
        sdesAlgebra.getSdesSubmissionsDestination().flatMap { stats =>
          stats.find(_.destination === to) match {
            case Some(destination) =>
              BadRequest(s"Invalid migration. 'DataStore' destination already exists: $destination").pure[Future]
            case None => runMigration(from, to)
          }
        }
      case _ =>
        BadRequest("Invalid migration. Only 'DataStore' -> 'DataStoreLegacy' or back is allowed").pure[Future]
    }
  }

  private def runMigration(from: String, to: String) =
    sdesAlgebra
      .sdesMigration(from, to)
      .map { modifiedCount =>
        Ok(modifiedCount.toString)
      }

  def historyById(correlationId: CorrelationId) = Action.async { _ =>
    for {
      histories <- sdesHistoryAlgebra.get(correlationId).map(_.sortBy(_.createdAt).reverse)
      historyNotification <-
        histories.traverse(h =>
          SdesHistoryNotification(
            h.notificationStatus,
            LocalDateTime.ofInstant(h.createdAt, ZoneOffset.UTC),
            h.failureReason,
            h.notifyRequest
          ).pure[Future]
        )
    } yield
      if (histories.nonEmpty) {
        val firstHistory = histories.head
        Ok(
          Json.toJson(
            SdesHistoryView(
              firstHistory.envelopeId,
              firstHistory.correlationId,
              firstHistory.fileName,
              historyNotification
            )
          )
        )
      } else NotFound
  }
}
