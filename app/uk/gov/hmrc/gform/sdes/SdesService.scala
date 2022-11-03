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

import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.repo.Repo
import org.slf4j.LoggerFactory
import play.api.libs.json.Json
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.sdes.{ FileAudit, FileChecksum, FileMetaData, SdesNotifyRequest, SdesSubmission }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.objectstore.client.ObjectSummaryWithMd5

import java.util.Base64
import scala.concurrent.{ ExecutionContext, Future }

trait SdesAlgebra[F[_]] {
  def notifySDES(envelopeId: EnvelopeId, objWithSummary: ObjectSummaryWithMd5)(implicit
    hc: HeaderCarrier
  ): F[Unit]
}

class SdesService(
  sdesConnector: SdesConnector,
  repoSdesSubmission: Repo[SdesSubmission],
  informationType: String,
  recipientOrSender: String,
  fileLocationUrl: String
)(implicit
  ec: ExecutionContext
) extends SdesAlgebra[Future] {
  private val logger = LoggerFactory.getLogger(getClass)

  override def notifySDES(envelopeId: EnvelopeId, objWithSummary: ObjectSummaryWithMd5)(implicit
    hc: HeaderCarrier
  ): Future[Unit] = {
    val sdesSubmission = SdesSubmission.createSdesSubmission(envelopeId)
    val notifyRequest = createNotifyRequest(objWithSummary, sdesSubmission._id.value)
    logger.debug(s"SDES notification request: ${Json.stringify(Json.toJson(notifyRequest))}")
    for {
      _ <- repoSdesSubmission.upsert(sdesSubmission).toFuture
      _ <- sdesConnector.notifySDES(notifyRequest)
    } yield ()
  }

  private def createNotifyRequest(
    objSummary: ObjectSummaryWithMd5,
    correlationId: String
  ): SdesNotifyRequest =
    SdesNotifyRequest(
      informationType,
      FileMetaData(
        recipientOrSender,
        objSummary.location.fileName,
        s"$fileLocationUrl${objSummary.location.asUri}",
        FileChecksum(value = Base64.getDecoder.decode(objSummary.contentMd5.value).map("%02x".format(_)).mkString),
        objSummary.contentLength,
        List()
      ),
      FileAudit(correlationId)
    )
}
