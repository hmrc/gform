/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.auditing

import play.api.mvc.Headers
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.bootstrap.audit.DefaultAuditConnector
import uk.gov.hmrc.play.bootstrap.config.DefaultHttpAuditEvent
import uk.gov.hmrc.play.bootstrap.filters.microservice.{ DefaultMicroserviceAuditFilter, MicroserviceAuditFilter }

import scala.concurrent.ExecutionContext

class AuditingModule(configModule: ConfigModule, akkaModule: AkkaModule)(implicit ec: ExecutionContext) {
  self =>

  val auditConnector: AuditConnector = new DefaultAuditConnector(configModule.auditingConfig)

  val microserviceAuditFilter: MicroserviceAuditFilter = new DefaultMicroserviceAuditFilter(
    configModule.controllerConfigs,
    auditConnector,
    new DefaultHttpAuditEvent(configModule.appConfig.appName),
    akkaModule.materializer
  )
}

object loggingHelpers {
  def cleanHeaders(headers: Headers) =
    s"headers: '${headers.remove("Authorization", "token", "customerId").toSimpleMap.toString()}'"
  def cleanHeaderCarrierHeader(hc: HeaderCarrier): String =
    s"headers, sessionId: '${hc.sessionId.getOrElse("")}', deviceId: '${hc.deviceID.getOrElse("")}' requestId: '${hc.requestId
      .getOrElse("")}', request chain: '${hc.requestChain.value}'"
}
