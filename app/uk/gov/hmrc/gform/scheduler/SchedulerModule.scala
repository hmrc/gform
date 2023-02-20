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

package uk.gov.hmrc.gform.scheduler

import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.scheduler.sdes.{ SdesQueuePollingService, SdesQueueService, SdesWorkItemRepo }
import uk.gov.hmrc.gform.sdes.SdesModule

import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

class SchedulerModule(
  configModule: ConfigModule,
  mongoModule: MongoModule,
  sdesModule: SdesModule,
  akkaModule: AkkaModule
)(implicit ex: ExecutionContext) {

  private val retryAfter: Long = configModule.configuration.getMillis("sdes.queue.retryAfter")
  private val pollLerLimit = configModule.typesafeConfig.getInt("sdes.poller.pollLimit")
  private val pollerInitialDelay: FiniteDuration =
    FiniteDuration(configModule.typesafeConfig.getDuration("sdes.poller.initialDelay").toNanos, TimeUnit.NANOSECONDS)
  private val pollerInterval: FiniteDuration =
    FiniteDuration(configModule.typesafeConfig.getDuration("sdes.poller.interval").toNanos, TimeUnit.NANOSECONDS)
  private val pollerEnabled: Boolean = configModule.typesafeConfig.getBoolean(s"sdes.poller.enabled")
  private val maxFailureCount = configModule.typesafeConfig.getInt(s"sdes.queue.maxFailureCount")

  private val sdesNotificationRepository = new SdesWorkItemRepo(mongoModule.mongoComponent)

  val sdesQueueService =
    new SdesQueueService(sdesModule.sdesService, sdesNotificationRepository, pollLerLimit, maxFailureCount, retryAfter)

  new SdesQueuePollingService(
    akkaModule.actorSystem,
    pollerInitialDelay,
    pollerInterval,
    pollerEnabled,
    sdesQueueService
  )
}
