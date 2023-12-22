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
import play.api.libs.json.Json
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.objectstore.ObjectStoreAlgebra
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination.DataStore
import uk.gov.hmrc.gform.sharedmodel.sdes.{ CorrelationId, NotificationStatus, SdesDestination, SdesSubmissionData }
import uk.gov.hmrc.objectstore.client.Path

import scala.concurrent.{ ExecutionContext, Future }

class SdesController(
  cc: ControllerComponents,
  sdesAlgebra: SdesAlgebra[Future],
  objectStoreAlgebra: ObjectStoreAlgebra[Future],
  sdesBasePath: String,
  dataStorebasePath: String
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
                      objSummary <- submission.destination match {
                                      case Some(DataStore) =>
                                        val fileName = s"${submission.envelopeId.value}.json"
                                        val envelopeDirectory = Path.Directory(
                                          s"${dataStorebasePath}envelopes/${submission.envelopeId.value}"
                                        )

                                        for {
                                          maybeObject <- objectStoreAlgebra.getFile(envelopeDirectory, fileName)
                                          objSummary <- maybeObject match {
                                                          case Some(obj) =>
                                                            val byteStringFuture: Future[ByteString] =
                                                              obj.content.runFold(ByteString.empty)(_ ++ _)

                                                            byteStringFuture.flatMap { concatenatedByteString =>
                                                              objectStoreAlgebra.uploadFile(
                                                                Path.Directory(
                                                                  s"$sdesBasePath$dataStorebasePath"
                                                                ),
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
                                      case _ => objectStoreAlgebra.zipFiles(submission.envelopeId)
                                    }
                      res <- sdesAlgebra.notifySDES(submission, objSummary)
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

  def sdesMigration() = Action.async { _ =>
    sdesAlgebra
      .sdesMigration()
      .map { modifiedCount =>
        Ok(modifiedCount.toString)
      }
  }

}
