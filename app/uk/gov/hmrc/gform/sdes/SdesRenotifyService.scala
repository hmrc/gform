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

import org.apache.pekko.stream.Materializer
import org.apache.pekko.util.ByteString
import uk.gov.hmrc.gform.objectstore.ObjectStoreAlgebra
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.sdes.{ CorrelationId, SdesDestination }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class SdesRenotifyService(
  sdesAlgebra: SdesAlgebra[Future],
  objectStoreAlgebra: ObjectStoreAlgebra[Future]
)(implicit ec: ExecutionContext, m: Materializer) {

  def renotifySDES(correlationId: CorrelationId)(implicit hc: HeaderCarrier) =
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
                                                              objectStoreAlgebra.uploadFileWithDir(
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
    } yield result
}
