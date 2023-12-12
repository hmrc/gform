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

import akka.stream.Materializer
import akka.util.ByteString
import cats.syntax.all._
import play.api.libs.json.Json
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.objectstore.ObjectStoreAlgebra
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.sdes.{ CorrelationId, NotificationStatus, SdesDestination, SdesSubmissionData }

import scala.concurrent.{ ExecutionContext, Future }

class SdesController(
  cc: ControllerComponents,
  sdesAlgebra: SdesAlgebra[Future],
  objectStoreAlgebra: ObjectStoreAlgebra[Future]
)(implicit
  ex: ExecutionContext,
  m: Materializer
) extends BaseController(cc) {

  def search(
    page: Int,
    pageSize: Int,
    processed: Option[Boolean],
    formTemplateId: Option[FormTemplateId],
    status: Option[NotificationStatus],
    showBeforeAt: Option[Boolean],
    destination: Option[SdesDestination]
  ) = Action.async { _ =>
    sdesAlgebra
      .search(page, pageSize, processed, formTemplateId, status, showBeforeAt, destination)
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
    for {
      sdesSubmission <- sdesAlgebra.findSdesSubmission(correlationId)
      result <- sdesSubmission match {
                  case Some(submission) =>
                    val sdesDestination = submission.sdesDestination
                    val paths = sdesDestination.objectStorePaths(submission.envelopeId)
                    for {
                      objSummary <- sdesDestination match {
                                      case SdesDestination.DataStore | SdesDestination.DataStoreLegacy |
                                          SdesDestination.HmrcIlluminate =>
                                        val fileName = s"${submission.envelopeId.value}.json"
                                        for {
                                          maybeObject <- objectStoreAlgebra.getFile(paths.permanent, fileName)
                                          objSummary <- maybeObject match {
                                                          case Some(obj) =>
                                                            val byteStringFuture: Future[ByteString] =
                                                              obj.content.runFold(ByteString.empty)(_ ++ _)

                                                            byteStringFuture.flatMap { concatenatedByteString =>
                                                              objectStoreAlgebra.uploadFile(
                                                                paths.ephemeral,
                                                                fileName,
                                                                concatenatedByteString,
                                                                ContentType.`application/json`
                                                              )
                                                            }
                                                          case None =>
                                                            Future.failed(
                                                              new Exception(
                                                                s"File ${submission.envelopeId.value}.json not found in the object store."
                                                              )
                                                            )
                                                        }
                                        } yield objSummary
                                      case SdesDestination.Dms =>
                                        objectStoreAlgebra.zipFiles(submission.envelopeId, paths)
                                    }
                      res <- sdesAlgebra.renotifySDES(submission, objSummary)
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
}
