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

import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core.{ FOpt, fOptMonadError }
import uk.gov.hmrc.gform.wshttp._
import uk.gov.hmrc.http.logging.Authorization
import uk.gov.hmrc.gform.wshttp.HttpClient.HttpClientBuildingSyntax

import scala.concurrent.ExecutionContext.Implicits.global

class HandlebarsHttpApiModule(wSHttpModule: WSHttpModule, configModule: ConfigModule) {

  private val rootHttpClient: HttpClient[FOpt] =
    new AuditingHttpClient(wSHttpModule.auditableWSHttp)

  private val desConfig = configModule.desConfig

  private val desHttpClient: JsonHttpClient[FOpt] =
    rootHttpClient
      .buildUri(uri => appendUriSegment(configModule.serviceConfig.baseUrl("etmp-hod"), uri))
      .buildHeaderCarrier(hc => {
        hc.copy(
          extraHeaders = ("Environment" -> desConfig.environment) :: hc.extraHeaders.toList,
          authorization = Some(Authorization(s"Bearer ${desConfig.authorizationToken}"))
        )
      })
      .json

  private val mdtpHttpClientMap: MdtpHttpClient[FOpt] = MdtpHttpClient(
    configModule.mdtpServiceConfigs.mapValues { configuration =>
      rootHttpClient
        .buildUri(uri => appendUriSegment(configuration.baseUrl, uri))
        .buildHeaderCarrier(hc => {
          val hc1 =
            configuration.authorizationToken.fold(hc)(at => hc.copy(authorization = Some(Authorization(s"Bearer $at"))))
          val hc2 = configuration.environment.fold(hc1)(e =>
            hc1.copy(extraHeaders = ("Environment" -> e) :: hc1.extraHeaders.toList))
          hc2
        })
        .json
    }
  )

  private def appendUriSegment(base: String, toAppend: String) =
    if (base.endsWith("/") || toAppend.startsWith("/")) s"$base$toAppend"
    else s"$base/$toAppend"

//  private val mdgConfig = configModule.mdgIntegrationFrameworkConfig

  // ToDo - Lance - Don't know what the properties are yet. Build as for the desHttpClient.
  private val mdgHttpClient: JsonHttpClient[FOpt] =
    HttpClient.unimplementedHttpClient[FOpt].json

  val handlebarsHttpSubmitter: HandlebarsHttpApiSubmitter[FOpt] =
    new RealHandlebarsHttpApiSubmitter(desHttpClient, mdgHttpClient, mdtpHttpClientMap)
}
