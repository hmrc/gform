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

package uk.gov.hmrc.gform.submission.handlebars

import cats.MonadError
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.submission.destinations._
import uk.gov.hmrc.gform.wshttp.HttpClient
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import play.api.libs.json._
import cats.syntax.all._
import scala.util.Try
import uk.gov.hmrc.gform.logging.Loggers

trait HandlebarsHttpApiSubmitter[F[_]] {
  def apply(
    destination: Destination.HandlebarsHttpApi,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree
  )(implicit hc: HeaderCarrier): F[HttpResponse]
}

class RealHandlebarsHttpApiSubmitter[F[_]](
  httpClients: Map[ProfileName, HttpClient[F]],
  handlebarsTemplateProcessor: HandlebarsTemplateProcessor = RealHandlebarsTemplateProcessor
)(implicit monadError: MonadError[F, String])
    extends HandlebarsHttpApiSubmitter[F] {

  def apply(
    destination: Destination.HandlebarsHttpApi,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree
  )(implicit hc: HeaderCarrier): F[HttpResponse] = {

    def parseAndProcessAsList(input: String): List[String] =
      Try(Json.parse(input)).toOption
        .flatMap {
          case jsonArray: JsArray => Some(jsonArray.value.map(_.toString).toList)
          case _                  => None
        }
        .getOrElse(List(input))
        .map(processPayload)

    def callUntilError(
      uri: String,
      verb: (String, String) => F[HttpResponse]
    ): F[HttpResponse] = {
      val destinationId = destination.id
      val formId = modelTree.value.formId
      val input: List[String] = destination.payload.fold(List(""))(body => parseAndProcessAsList(body))

      def logAndRaiseError(message: String): F[HttpResponse] =
        logError(message) *> monadError.raiseError(message)

      def logInfo(message: String): F[Unit] =
        monadError.pure(Loggers.destinations.info(genericLogMessage(formId, destinationId, message)))

      def logError(message: String): F[Unit] =
        monadError.pure(Loggers.destinations.error(genericLogMessage(formId, destinationId, message)))

      def callUrlBodyPairs(uri: String, bodies: List[String], lastResult: Option[HttpResponse]): F[HttpResponse] =
        bodies match {
          case Nil => lastResult.fold(logAndRaiseError("No input provided"))(monadError.pure)
          case body :: rest =>
            val resultOrError = for {
              result <- monadError.attempt(verb(uri, body))
              _      <- lastResult.fold(monadError.pure(()))(_ => logInfo("Successfully applied verb method"))
            } yield result

            resultOrError.flatMap {
              case Right(result) => callUrlBodyPairs(uri, rest, Some(result))
              case Left(error)   => logAndRaiseError(error)
            }
        }

      callUrlBodyPairs(uri, input, None)
    }

    def processPayload(template: String): String =
      handlebarsTemplateProcessor(
        template,
        accumulatedModel,
        FocussedHandlebarsModelTree(modelTree),
        destination.payloadType
      )

    RealHandlebarsHttpApiSubmitter
      .selectHttpClient(destination.profile, destination.payloadType, httpClients)
      .flatMap { httpClient =>
        val uri =
          handlebarsTemplateProcessor(
            destination.uri,
            accumulatedModel,
            FocussedHandlebarsModelTree(modelTree),
            TemplateType.Plain
          )

        if (destination.multiRequestPayload) {
          destination.method match {
            case HttpMethod.GET  => httpClient.get(uri)
            case HttpMethod.POST => callUntilError(uri, httpClient.post)
            case HttpMethod.PUT  => callUntilError(uri, httpClient.put)
          }
        } else {
          destination.method match {
            case HttpMethod.GET => httpClient.get(uri)
            case HttpMethod.POST =>
              val body = destination.payload.fold("")(processPayload)
              println(s"body: $body")
              httpClient.post(uri, body)
            case HttpMethod.PUT =>
              val body = destination.payload.fold("")(processPayload)
              httpClient.put(uri, body)
          }
        }
      }
  }

}

object RealHandlebarsHttpApiSubmitter {
  def selectHttpClient[F[_]](
    profile: ProfileName,
    payloadType: TemplateType,
    httpClients: Map[ProfileName, HttpClient[F]]
  )(implicit me: MonadError[F, String]): F[HttpClient[F]] =
    httpClients
      .get(profile)
      .fold(
        me.raiseError[HttpClient[F]](
          s"No HttpClient found for profile ${profile.name}. Have HttpClient for ${httpClients.keySet.map(_.name)}"
        )
      )((c: HttpClient[F]) => wrapHttpClient(c, payloadType).pure)

  private def wrapHttpClient[F[_]](http: HttpClient[F], templateType: TemplateType)(implicit
    me: MonadError[F, String]
  ): HttpClient[F] =
    templateType match {
      case TemplateType.JSON  => http.json
      case TemplateType.XML   => http.xml
      case TemplateType.Plain => http
    }
}
