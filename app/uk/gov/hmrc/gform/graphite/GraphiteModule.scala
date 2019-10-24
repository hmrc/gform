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

package uk.gov.hmrc.gform.graphite

import com.codahale.metrics.MetricFilter
import com.codahale.metrics.graphite.GraphiteReporter
import com.kenshoo.play.metrics.MetricsImpl
import play.api.inject.ApplicationLifecycle
import play.api.{ Configuration, Environment }
import uk.gov.hmrc.play.bootstrap.config.RunMode
import uk.gov.hmrc.play.bootstrap.graphite.{ EnabledGraphiteReporting, GraphiteProvider, GraphiteProviderConfig, GraphiteReporterProvider, GraphiteReporterProviderConfig }

class GraphiteModule(
  environment: Environment,
  configuration: Configuration,
  runMode: RunMode,
  applicationLifecycle: ApplicationLifecycle) {
  // Taken from uk.gov.hmrc.play.bootstrap.graphite.GraphiteMetricsModule of bootstrap-play-26 library
  private def extractGraphiteConfiguration(environment: Environment, configuration: Configuration): Configuration = {
    val env = runMode.env
    configuration
      .getOptional[Configuration](s"$env.microservice.metrics.graphite")
      .orElse(configuration.getOptional[Configuration]("microservice.metrics.graphite"))
      .getOrElse(Configuration())
  }

  private val graphiteConfiguration: Configuration = extractGraphiteConfiguration(environment, configuration)

  private val enableGraphite = graphiteConfiguration.getOptional[Boolean]("enabled").getOrElse(false)

  if (enableGraphite) {
    val config: GraphiteReporterProviderConfig =
      GraphiteReporterProviderConfig.fromConfig(configuration, graphiteConfiguration)
    val graphiteProviderConfig: GraphiteProviderConfig = GraphiteProviderConfig.fromConfig(graphiteConfiguration)
    val graphite = new GraphiteProvider(graphiteProviderConfig).get()
    val filter: MetricFilter = MetricFilter.ALL
    val metrics = new MetricsImpl(applicationLifecycle, configuration)
    val graphiteReporter: GraphiteReporter = new GraphiteReporterProvider(config, metrics, graphite, filter).get()
    new EnabledGraphiteReporting(configuration, graphiteReporter, applicationLifecycle)
  }
}
