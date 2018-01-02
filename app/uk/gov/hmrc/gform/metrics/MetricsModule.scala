/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.metrics

import com.kenshoo.play.metrics.{ MetricsController, MetricsFilter, MetricsFilterImpl, MetricsImpl }
import play.api.ApplicationLoader
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.playcomponents.PlayComponents

class MetricsModule(playComponents: PlayComponents, akkaModule: AkkaModule) {

  // Don't use uk.gov.hmrc.play.graphite.GraphiteMetricsImpl as it won't allow hot reload due to overridden onStop() method
  val metrics = new MetricsImpl(
    playComponents.context.lifecycle,
    playComponents.context.initialConfiguration)

  val metricsFilter: MetricsFilter = new MetricsFilterImpl(metrics)(akkaModule.materializer)

  val metricsController = new MetricsController(metrics)

}
