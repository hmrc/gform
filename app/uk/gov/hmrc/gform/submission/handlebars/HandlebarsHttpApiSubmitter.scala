/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.submission.handlebars

import org.slf4j.LoggerFactory
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.submission.destinations._
import uk.gov.hmrc.http.client.RequestBuilder
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.http.HttpReads.Implicits._

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

trait HandlebarsHttpApiSubmitter {
  def apply(
    destination: Destination.HandlebarsHttpApi,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    submissionInfo: DestinationSubmissionInfo
  )(implicit hc: HeaderCarrier): Future[HttpResponse]
}

class RealHandlebarsHttpApiSubmitter(
  buildRequest: (ProfileName, EnvelopeId, String, HttpMethod) => RequestBuilder,
  handlebarsTemplateProcessor: HandlebarsTemplateProcessor = RealHandlebarsTemplateProcessor
)(implicit ec: ExecutionContext)
    extends HandlebarsHttpApiSubmitter {

  private val logger = LoggerFactory.getLogger(getClass)

  def apply(
    destination: Destination.HandlebarsHttpApi,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    submissionInfo: DestinationSubmissionInfo
  )(implicit hc: HeaderCarrier): Future[HttpResponse] = {

    val envelopeId = submissionInfo.submission.envelopeId
    val uri = handlebarsTemplateProcessor(
      destination.uri,
      accumulatedModel,
      FocussedHandlebarsModelTree(modelTree),
      TemplateType.Plain
    )
    val requestBuilder = buildRequest(destination.profile, envelopeId, uri, destination.method)

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

    def send(body: String): Future[HttpResponse] =
      requestBuilder
        .setHeader("Content-Type" -> contentType)
        .withBody(body)
        .execute[HttpResponse]

    def sendMultipleRequests(bodies: List[String]): Future[HttpResponse] =
      if (bodies.isEmpty) {
        send("")
      } else {
        def sendNext(remaining: List[String], lastResponse: Option[HttpResponse]): Future[HttpResponse] =
          remaining match {
            case Nil =>
              lastResponse match {
                case Some(response) => Future.successful(response)
                case None           => Future.failed(new Exception("No successful responses"))
              }
            case body :: rest =>
              send(body)
                .flatMap { response =>
                  sendNext(rest, Some(response))
                }
                .recoverWith { case ex =>
                  logger.error(s"Error submitting to ${destination.id}: ${ex.getMessage}", ex)
                  if (rest.nonEmpty) {
                    sendNext(rest, lastResponse)
                  } else {
                    lastResponse match {
                      case Some(response) => Future.successful(response)
                      case None           => Future.failed(ex)
                    }
                  }
                }
          }
        sendNext(bodies, None)
      }

    val payloads: List[String] = destination.payload match {
      case Some(body) if destination.multiRequestPayload => parseAndProcessAsList(body)
      case Some(body)                                    => List(processPayload(body))
      case None                                          => List("")
    }

    destination.method match {
      case HttpMethod.GET =>
        buildRequest(destination.profile, envelopeId, uri, destination.method)
          .execute[HttpResponse]

      case HttpMethod.POST =>
        if (destination.multiRequestPayload) sendMultipleRequests(payloads)
        else send(payloads.headOption.getOrElse(""))

      case HttpMethod.PUT =>
        if (destination.multiRequestPayload) sendMultipleRequests(payloads)
        else send(payloads.headOption.getOrElse(""))
    }
  }
}
