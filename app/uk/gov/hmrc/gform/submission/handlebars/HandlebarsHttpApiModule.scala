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

import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core.{ FOpt, fOptMonadError }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.ProfileName
import uk.gov.hmrc.gform.wshttp._
import uk.gov.hmrc.gform.wshttp.HttpClient.HttpClientBuildingSyntax

import java.time.temporal.ChronoUnit
import java.time.{ ZoneOffset, ZonedDateTime }
import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.util.matching.Regex

class HandlebarsHttpApiModule(
  wSHttpModule: WSHttpModule,
  configModule: ConfigModule
)(implicit ec: ExecutionContext) {

  private val rootHttpClient: HttpClient[FOpt] = wSHttpModule.auditingHttpClient
  private val checkToken: Regex = "^\\{(.*)}$".r

  private val httpClientMap: Map[ProfileName, HttpClient[FOpt]] =
    configModule
      .DestinationsServicesConfig()
      .view
      .mapValues { profileConfiguration =>
        rootHttpClient
          .buildUri(uri => appendUriSegment(profileConfiguration.baseUrl, uri))
          .buildHeaderCarrier { hc =>
            hc.copy(
              authorization = profileConfiguration.authorization orElse hc.authorization,
              extraHeaders = hc.extraHeaders ++ profileConfiguration.httpHeaders.map {
                case (k, checkToken(v)) => k -> getDynamicValue(v)
                case otherwise          => otherwise
              }
            )
          }
      }
      .toMap

  private def getDynamicValue(token: String): String = token match {
    case "UUID"    => UUID.randomUUID().toString
    case "NOW"     => ZonedDateTime.now(ZoneOffset.UTC).truncatedTo(ChronoUnit.SECONDS).toString
    case otherwise => otherwise
  }

  private def appendUriSegment(base: String, toAppend: String) =
    if (toAppend.isEmpty) base
    else if (base.endsWith("/") || toAppend.startsWith("/")) s"$base$toAppend"
    else s"$base/$toAppend"

  val handlebarsHttpSubmitter: HandlebarsHttpApiSubmitter[FOpt] =
    new RealHandlebarsHttpApiSubmitter(httpClientMap)
}
