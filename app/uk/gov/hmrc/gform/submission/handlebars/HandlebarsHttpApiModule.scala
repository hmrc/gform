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

import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.wshttp._
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.http.logging.Authorization

import scala.concurrent.ExecutionContext.Implicits.global

class HandlebarsHttpApiModule(wSHttpModule: WSHttpModule, configModule: ConfigModule) {

  private val rootHttpClient: HttpClient[FOpt] =
    new AuditingHttpClient(wSHttpModule.auditableWSHttp)

  private val desConfig = configModule.desConfig

  import cats.instances.future._
  private val desHttpClient =
    new UriBuildingHttpClient(
      uri => s"${configModule.serviceConfig.baseUrl("etmp-hod")}${desConfig.basePath}/$uri",
      new HeaderCarrierBuildingHttpClient(
        _ => {
          HeaderCarrier(
            extraHeaders = Seq("Environment" -> desConfig.environment),
            authorization = Some(Authorization(s"Bearer ${desConfig.authorizationToken}")))
        },
        new LoggingHttpClient(LoggerFactory.getLogger("connector"), rootHttpClient)
      )
    )

//  private val mdgConfig = configModule.mdgIntegrationFrameworkConfig

  // ToDo - Lance - Don't know what the properties are yet. Build as for the desHttpClient.
  private val mdgHttpClient: HttpClient[FOpt] = new HttpClient[FOpt] {
    override def get(uri: String)(implicit hc: HeaderCarrier): FOpt[HttpResponse] = ???
    override def postJson(uri: String, json: String)(implicit hc: HeaderCarrier): FOpt[HttpResponse] = ???
  }

  val handlebarsHttpSubmitter: HandlebarsHttpApiSubmitter[FOpt] =
    new RealHandlebarsHttpApiSubmitter(desHttpClient, mdgHttpClient)
}
