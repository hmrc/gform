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
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.scheduler.TraceableWorkItem
import uk.gov.hmrc.gform.scheduler.asynchandlebars.{ AsyncHandlebarsWorkItem, AsyncHandlebarsWorkItemBuilder, AsyncHandlebarsWorkItemRepo }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.submission.handlebars.{ HandlebarsModelTree, HandlebarsTemplateProcessor }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.workitem.ProcessingStatus

import scala.concurrent.ExecutionContext

trait AsyncHttpWorkItemSubmitter[F[_]] {
  def apply(
    destination: Destination.AsyncHandlebarsHttpApi,
    destinationHttpHeaders: Map[String, String],
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    submissionInfo: DestinationSubmissionInfo,
    handlebarsTemplateProcessor: HandlebarsTemplateProcessor
  )(implicit hc: HeaderCarrier): F[DestinationResponse]
}

class RealAsyncHttpWorkItemSubmitter(
  asyncHandlebarsWorkItemRepo: AsyncHandlebarsWorkItemRepo
)(implicit ec: ExecutionContext)
    extends AsyncHttpWorkItemSubmitter[FOpt] {

  private val logger = LoggerFactory.getLogger(getClass)

  private def deferred(item: TraceableWorkItem[_]): ProcessingStatus = ProcessingStatus.Deferred

  def apply(
    destination: Destination.AsyncHandlebarsHttpApi,
    destinationHttpHeaders: Map[String, String],
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    submissionInfo: DestinationSubmissionInfo,
    handlebarsTemplateProcessor: HandlebarsTemplateProcessor
  )(implicit hc: HeaderCarrier): FOpt[DestinationResponse] = {
    val renderSnapshot = AsyncHandlebarsWorkItemBuilder.createRenderSnapshot(accumulatedModel, modelTree)

    val workItem = TraceableWorkItem[AsyncHandlebarsWorkItem](
      envelopeId = submissionInfo.submission.envelopeId,
      formTemplateId = submissionInfo.submission.dmsMetaData.formTemplateId,
      submissionRef = submissionInfo.submission.submissionRef,
      destinationId = destination.id,
      data = AsyncHandlebarsWorkItemBuilder.build(
        destination,
        destinationHttpHeaders,
        accumulatedModel,
        modelTree,
        Some(renderSnapshot),
        handlebarsTemplateProcessor
      )
    )

    JsonSchemaValidationSupport
      .validatePayload(destination, workItem.data.payload)
      .fold(
        message =>
          throw new RuntimeException(s"Schema validation failed for destination '${destination.id.id}': $message"),
        _ => ()
      )

    logger.debug(
      s"Submitting async HTTP work item for form template ${workItem.formTemplateId.value}, destination id ${workItem.destinationId.id}, URI: ${workItem.data.uri}, method: ${workItem.data.method}, content type: ${workItem.data.contentType.value}"
    )

    fromFutureA(
      asyncHandlebarsWorkItemRepo
        .pushNew(workItem, initialState = deferred)
        .map(item => AsyncHandlebarsDestinationResponse(item.id))
    )
  }
}
