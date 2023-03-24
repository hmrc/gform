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

package uk.gov.hmrc.gform.graphite

import com.codahale.metrics.graphite.{ Graphite, GraphiteReporter }
import com.codahale.metrics.MetricFilter
import play.api.inject.ApplicationLifecycle
import play.api.{ Configuration, Environment }
import uk.gov.hmrc.gform.metrics.MetricsModule
import uk.gov.hmrc.play.audit.http.connector.DatastreamMetrics
import uk.gov.hmrc.play.bootstrap.audit.{ DisabledDatastreamMetricsProvider, EnabledDatastreamMetricsProvider }
import uk.gov.hmrc.play.bootstrap.graphite.{ DisabledGraphiteReporting, EnabledGraphiteReporting, GraphiteProvider, GraphiteProviderConfig, GraphiteReporterProvider, GraphiteReporterProviderConfig }

class GraphiteModule(
  environment: Environment,
  configuration: Configuration,
  applicationLifecycle: ApplicationLifecycle,
  metricsModule: MetricsModule
) {

  // ============================
  // Taken from uk.gov.hmrc.play.bootstrap.graphite.GraphiteMetricsModule of bootstrap-play-26 library
  private def kenshooMetricsEnabled(rootConfiguration: Configuration) =
    rootConfiguration.getOptional[Boolean]("metrics.enabled").getOrElse(false)

  private def graphitePublisherEnabled(graphiteConfiguration: Configuration) =
    graphiteConfiguration.getOptional[Boolean]("enabled").getOrElse(false)

  private def extractGraphiteConfiguration(configuration: Configuration): Configuration =
    configuration
      .getOptional[Configuration]("microservice.metrics.graphite")
      .getOrElse(Configuration())

  private val graphiteConfiguration: Configuration = extractGraphiteConfiguration(configuration)
  // ==========================

  val datastreamMetrics: DatastreamMetrics =
    if (kenshooMetricsEnabled(configuration) && graphitePublisherEnabled(graphiteConfiguration)) {

      val graphiteProviderConfig: GraphiteProviderConfig = GraphiteProviderConfig.fromRootConfig(graphiteConfiguration)

      val reporterConfig: GraphiteReporterProviderConfig =
        GraphiteReporterProviderConfig.fromConfig(configuration)

      val graphite: Graphite = new GraphiteProvider(graphiteProviderConfig).get

      val graphiteReporter: GraphiteReporter = new GraphiteReporterProvider(
        config = reporterConfig,
        metrics = metricsModule.metrics,
        graphite = graphite,
        filter = MetricFilter.ALL
      ).get

      // Runs side effects
      new EnabledGraphiteReporting(configuration, graphiteReporter, applicationLifecycle)

      new EnabledDatastreamMetricsProvider(reporterConfig, metricsModule.metrics).get()

    } else {
      // Runs side effects
      new DisabledGraphiteReporting()

      new DisabledDatastreamMetricsProvider().get()
    }

}
