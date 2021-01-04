/*
 * Copyright 2021 HM Revenue & Customs
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
import cats.syntax.applicative._
import cats.syntax.flatMap._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.wshttp.HttpClient
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

trait HandlebarsHttpApiSubmitter[F[_]] {
  def apply(
    destination: Destination.HandlebarsHttpApi,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree)(implicit hc: HeaderCarrier): F[HttpResponse]
}

class RealHandlebarsHttpApiSubmitter[F[_]](
  httpClients: Map[ProfileName, HttpClient[F]],
  handlebarsTemplateProcessor: HandlebarsTemplateProcessor = RealHandlebarsTemplateProcessor)(
  implicit me: MonadError[F, String])
    extends HandlebarsHttpApiSubmitter[F] {

  def apply(
    destination: Destination.HandlebarsHttpApi,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree)(implicit hc: HeaderCarrier): F[HttpResponse] =
    RealHandlebarsHttpApiSubmitter
      .selectHttpClient(destination.profile, destination.payloadType, httpClients)
      .flatMap { httpClient =>
        val uri =
          handlebarsTemplateProcessor(
            destination.uri,
            accumulatedModel,
            FocussedHandlebarsModelTree(modelTree),
            TemplateType.Plain)
        destination.method match {
          case HttpMethod.GET => httpClient.get(uri)
          case HttpMethod.POST =>
            val body = destination.payload.fold("") {
              handlebarsTemplateProcessor(
                _,
                accumulatedModel,
                FocussedHandlebarsModelTree(modelTree),
                destination.payloadType)
            }
            httpClient.post(uri, body)
          case HttpMethod.PUT =>
            val body = destination.payload.fold("") {
              handlebarsTemplateProcessor(
                _,
                accumulatedModel,
                FocussedHandlebarsModelTree(modelTree),
                destination.payloadType)
            }
            httpClient.put(uri, body)
        }
      }
}

object RealHandlebarsHttpApiSubmitter {
  def selectHttpClient[F[_]](
    profile: ProfileName,
    payloadType: TemplateType,
    httpClients: Map[ProfileName, HttpClient[F]])(implicit me: MonadError[F, String]): F[HttpClient[F]] =
    httpClients
      .get(profile)
      .fold(me.raiseError[HttpClient[F]](
        s"No HttpClient found for profile ${profile.name}. Have HttpClient for ${httpClients.keySet.map(_.name)}"))(
        (c: HttpClient[F]) => wrapHttpClient(c, payloadType).pure)

  private def wrapHttpClient[F[_]](http: HttpClient[F], templateType: TemplateType)(
    implicit me: MonadError[F, String]): HttpClient[F] =
    templateType match {
      case TemplateType.JSON  => http.json
      case TemplateType.XML   => http.xml
      case TemplateType.Plain => http
    }
}
