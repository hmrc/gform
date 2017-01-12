/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.connectors

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.concurrent.{ ExecutionContext, Future }
import play.api.libs.json.{ JsObject, JsValue, Json }
import play.api.http.HeaderNames
import uk.gov.hmrc.play.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.bforms.model.{ FileId, EnvelopeId }
import uk.gov.hmrc.bforms.exceptions.{ UnexpectedState, InvalidState }
import uk.gov.hmrc.bforms.typeclasses.{ CreateEnvelope, UploadFile, FusFeUrl, HttpExecutor, ServiceUrl }

class FusFeConnector() {

  /* def uploadFile(
   *   envelopeId: EnvelopeId,
   *   fileId: FileId,
   *   file: Array[Byte]
   * )(
   *   implicit
   *   hc: HeaderCarrier,
   *   ec: ExecutionContext,
   *   fusFeUrl: ServiceUrl[FusFeUrl],
   *   httpExecutor: HttpExecutor[UploadFile, Array[Byte]]
   * ): Future[Either[UnexpectedState, EnvelopeId]] = {
   *   httpExecutor.makeCall(UploadFile(envelopeId, fileId), file)
   *     .map { resp =>
   *       println("resp.body " + resp.body)
   *       println("resp.status " + resp.status)
   *       resp.allHeaders.foreach(println)
   *       import HeaderNames._
   *       resp.header(LOCATION) match {
   *         case Some(location) => Right(EnvelopeId("envelopeId"))
   *         case None => Left(InvalidState(s"Header $LOCATION not found"))
   *       }
   *     }
   * } */
}
