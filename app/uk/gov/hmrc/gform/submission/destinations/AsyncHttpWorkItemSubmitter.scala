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

import play.api.libs.json.{ JsArray, Json }
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.scheduler.asynchandlebars.AsyncHandlebarsWorkItem
import uk.gov.hmrc.gform.sdes.workitem.DestinationWorkItemAlgebra
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.submission.handlebars.{ FocussedHandlebarsModelTree, HandlebarsModelTree, HandlebarsTemplateProcessor }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.util.Try

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

  def apply(
    destination: Destination.AsyncHandlebarsHttpApi,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    submissionInfo: DestinationSubmissionInfo,
    handlebarsTemplateProcessor: HandlebarsTemplateProcessor
  )(implicit hc: HeaderCarrier): FOpt[DestinationResponse] = {
    val envelopeId = submissionInfo.submission.envelopeId
    val uri = handlebarsTemplateProcessor(
      destination.uri,
      accumulatedModel,
      FocussedHandlebarsModelTree(modelTree),
      TemplateType.Plain
    )

    val contentType = destination.payloadType match {
      case TemplateType.JSON  => "application/json"
      case TemplateType.XML   => "application/xml"
      case TemplateType.Plain => "text/plain"
    }

    def processPayload(template: String): String =
      handlebarsTemplateProcessor(
        template,
        accumulatedModel,
        FocussedHandlebarsModelTree(modelTree),
        destination.payloadType
      )

    def parseAndProcessAsList(input: String): List[String] =
      Try(Json.parse(input)).toOption
        .flatMap {
          case arr: JsArray => Some(arr.value.map(_.toString).toList)
          case _            => None
        }
        .getOrElse(List(input))
        .map(processPayload)

    val payloads: List[String] = destination.payload match {
      case Some(body) if destination.multiRequestPayload => parseAndProcessAsList(body)
      case Some(body)                                    => List(processPayload(body))
      case None                                          => List("")
    }

    val workItem = AsyncHandlebarsWorkItem(
      envelopeId = envelopeId,
      formTemplateId = submissionInfo.submission.dmsMetaData.formTemplateId,
      submissionRef = submissionInfo.submission.submissionRef,
      profile = destination.profile,
      uri = uri,
      method = destination.method,
      contentType = contentType,
      payloads = payloads
    )

    destinationWorkItemAlgebra
      .pushAsyncHandlebarsWorkItem(workItem)
      .map(oid => AsyncHandlebarsDestinationResponse(oid))
  }
}
