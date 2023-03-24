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

package uk.gov.hmrc.gform.auditing

import play.api.inject.ApplicationLifecycle
import play.api.mvc.Headers
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.graphite.GraphiteModule
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.{ AuditConnector, DatastreamMetrics }
import uk.gov.hmrc.play.audit.{ DefaultAuditChannel, DefaultAuditConnector }
import uk.gov.hmrc.play.bootstrap.backend.filters.{ BackendAuditFilter, DefaultBackendAuditFilter }
import uk.gov.hmrc.play.bootstrap.config.DefaultHttpAuditEvent

import scala.concurrent.ExecutionContext

class AuditingModule(
  configModule: ConfigModule,
  graphiteModule: GraphiteModule,
  akkaModule: AkkaModule,
  applicationLifecycle: ApplicationLifecycle
)(implicit
  ec: ExecutionContext
) {
  self =>

  val datastreamMetrics: DatastreamMetrics = graphiteModule.datastreamMetrics

  val defaultAuditChannel =
    new DefaultAuditChannel(
      configModule.auditingConfig,
      akkaModule.materializer,
      applicationLifecycle,
      datastreamMetrics
    )

  val auditConnector: AuditConnector =
    new DefaultAuditConnector(
      configModule.auditingConfig,
      defaultAuditChannel,
      applicationLifecycle,
      datastreamMetrics
    )

  val microserviceAuditFilter: BackendAuditFilter = new DefaultBackendAuditFilter(
    configModule.configuration,
    configModule.controllerConfigs,
    auditConnector,
    new DefaultHttpAuditEvent(configModule.appConfig.appName),
    akkaModule.materializer
  )
}

object loggingHelpers {
  def cleanHeaders(headers: Headers) =
    s"headers: '${headers.remove("Authorization", "token", "customerId").toSimpleMap.toString()}'"
  def cleanHeaderCarrierHeader(hc: HeaderCarrier): String = {
    val sessionId = hc.sessionId.getOrElse("")
    val deviceId = hc.deviceID.getOrElse("")
    val requestId = hc.requestId.getOrElse("")
    val requestChain = hc.requestChain.value
    s"headers, sessionId: '$sessionId', deviceId: '$deviceId' requestId: '$requestId', request chain: '$requestChain'"
  }
}
