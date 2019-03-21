/*
 * Copyright 2019 HM Revenue & Customs
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

import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, HandlebarsTemplateProcessorModel, HttpMethod, Profile }
import uk.gov.hmrc.gform.wshttp.JsonHttpClient
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

trait HandlebarsHttpApiSubmitter[F[_]] {
  def apply(destination: Destination.HandlebarsHttpApi, model: HandlebarsTemplateProcessorModel)(
    implicit hc: HeaderCarrier): F[HttpResponse]
}

class RealHandlebarsHttpApiSubmitter[F[_]](
  des: JsonHttpClient[F],
  mdg: JsonHttpClient[F],
  mdtp: MdtpHttpClient[F],
  handlebarsTemplateProcessor: HandlebarsTemplateProcessor = new RealHandlebarsTemplateProcessor)
    extends HandlebarsHttpApiSubmitter[F] {

  def apply(destination: Destination.HandlebarsHttpApi, model: HandlebarsTemplateProcessorModel)(
    implicit hc: HeaderCarrier): F[HttpResponse] = {
    val httpClient = RealHandlebarsHttpApiSubmitter
      .selectHttpClient(destination.profile, des, mdg, mdtp)

    val uri = handlebarsTemplateProcessor(destination.uri, model)
    destination.method match {
      case HttpMethod.GET => httpClient.get(uri)
      case HttpMethod.POST =>
        val body = destination.payload.fold("") {
          handlebarsTemplateProcessor(_, model)
        }
        httpClient.postJsonString(uri, body)
      case HttpMethod.PUT =>
        val body = destination.payload.fold("") {
          handlebarsTemplateProcessor(_, model)
        }
        httpClient.putJsonString(uri, body)
    }
  }
}

object RealHandlebarsHttpApiSubmitter {
  def selectHttpClient[F[_]](
    profile: Profile,
    des: JsonHttpClient[F],
    mdg: JsonHttpClient[F],
    mdtp: MdtpHttpClient[F]): JsonHttpClient[F] =
    profile match {
      case Profile.DES                     => des
      case Profile.MdgIntegrationFramework => mdg
      case m: Profile.MDTP                 => mdtp.select(MdtpServiceName(m.serviceName))
    }
}
