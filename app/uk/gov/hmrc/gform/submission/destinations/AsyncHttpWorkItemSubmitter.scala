/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.submission.destinations

import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.scheduler.asynchandlebars.AsyncHandlebarsWorkItem
import uk.gov.hmrc.gform.sdes.workitem.DestinationWorkItemAlgebra
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.sharedmodel.sdes.CorrelationId
import uk.gov.hmrc.gform.submission.handlebars.{ FocussedHandlebarsModelTree, HandlebarsModelTree, HandlebarsTemplateProcessor }
import uk.gov.hmrc.http.HeaderCarrier

import java.util.UUID
import scala.concurrent.ExecutionContext

trait AsyncHttpWorkItemSubmitter[F[_]] {
  def apply(
    destination: Destination.AsyncHandlebarsHttpApi,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    submissionInfo: DestinationSubmissionInfo,
    handlebarsTemplateProcessor: HandlebarsTemplateProcessor
  )(implicit hc: HeaderCarrier): F[DestinationResponse]
}

class RealAsyncHttpWorkItemSubmitter(
  destinationWorkItemAlgebra: DestinationWorkItemAlgebra[FOpt]
)(implicit ec: ExecutionContext)
    extends AsyncHttpWorkItemSubmitter[FOpt] {

  private val logger = LoggerFactory.getLogger(getClass)

  def apply(
    destination: Destination.AsyncHandlebarsHttpApi,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    submissionInfo: DestinationSubmissionInfo,
    handlebarsTemplateProcessor: HandlebarsTemplateProcessor
  )(implicit hc: HeaderCarrier): FOpt[DestinationResponse] = {
    val uri = handlebarsTemplateProcessor(
      destination.uri,
      accumulatedModel,
      FocussedHandlebarsModelTree(modelTree),
      TemplateType.Plain
    )

    val contentType = destination.payloadType match {
      case TemplateType.JSON  => ContentType.`application/json`
      case TemplateType.XML   => ContentType.`application/xml`
      case TemplateType.Plain => ContentType.`text/plain`
    }

    def processPayload(template: String): String =
      handlebarsTemplateProcessor(
        template,
        accumulatedModel,
        FocussedHandlebarsModelTree(modelTree),
        destination.payloadType
      )

    val payload: String = destination.payload match {
      case Some(body) => processPayload(body)
      case None       => ""
    }

    val workItem = AsyncHandlebarsWorkItem(
      envelopeId = submissionInfo.submission.envelopeId,
      correlationId = CorrelationId(UUID.randomUUID().toString),
      formTemplateId = submissionInfo.submission.dmsMetaData.formTemplateId,
      submissionRef = submissionInfo.submission.submissionRef,
      destinationId = destination.id,
      profile = destination.profile,
      uri = uri,
      method = destination.method,
      contentType = contentType,
      payload = payload
    )

    logger.debug(
      s"Submitting async HTTP work item for form template ${workItem.formTemplateId.value}, destination id ${workItem.destinationId.id}, URI: ${workItem.uri}, method: ${workItem.method}, content type: ${workItem.contentType.value}"
    )

    destinationWorkItemAlgebra
      .pushAsyncHandlebarsWorkItem(workItem)
      .map(oid => AsyncHandlebarsDestinationResponse(oid))
  }
}
