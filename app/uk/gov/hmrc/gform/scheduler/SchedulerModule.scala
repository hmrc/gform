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
import uk.gov.hmrc.gform.scheduler.datastore.{ DataStoreQueuePollingService, DataStoreQueueService, DataStoreWorkItemRepo }
import uk.gov.hmrc.gform.scheduler.dms.{ DmsQueuePollingService, DmsQueueService, DmsWorkItemRepo }
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

  private val dmsRetryAfter: Long = configModule.configuration.getMillis("sdes.dms.queue.retryAfter")
  private val dmsPollLerLimit = configModule.typesafeConfig.getInt("sdes.dms.poller.pollLimit")
  private val dmsPollerInitialDelay: FiniteDuration = FiniteDuration(
    configModule.typesafeConfig.getDuration("sdes.dms.poller.initialDelay").toNanos,
    TimeUnit.NANOSECONDS
  )
  private val dmsPollerInterval: FiniteDuration =
    FiniteDuration(configModule.typesafeConfig.getDuration("sdes.dms.poller.interval").toNanos, TimeUnit.NANOSECONDS)
  private val dmsPollerEnabled: Boolean = configModule.typesafeConfig.getBoolean(s"sdes.dms.poller.enabled")
  private val dmsMaxFailureCount = configModule.typesafeConfig.getInt(s"sdes.dms.queue.maxFailureCount")

  private val dmsNotificationRepository = new DmsWorkItemRepo(mongoModule.mongoComponent)

  val dmsQueueService =
    new DmsQueueService(
      sdesModule.sdesService,
      dmsNotificationRepository,
      dmsPollLerLimit,
      dmsMaxFailureCount,
      dmsRetryAfter
    )

  new DmsQueuePollingService(
    akkaModule.actorSystem,
    dmsPollerInitialDelay,
    dmsPollerInterval,
    dmsPollerEnabled,
    dmsQueueService
  )

  private val dataStoreRetryAfter: Long = configModule.configuration.getMillis("sdes.data-store.queue.retryAfter")
  private val dataStorePollLerLimit = configModule.typesafeConfig.getInt("sdes.data-store.poller.pollLimit")
  private val dataStorePollerInitialDelay: FiniteDuration =
    FiniteDuration(
      configModule.typesafeConfig.getDuration("sdes.data-store.poller.initialDelay").toNanos,
      TimeUnit.NANOSECONDS
    )
  private val dataStorePollerInterval: FiniteDuration =
    FiniteDuration(
      configModule.typesafeConfig.getDuration("sdes.data-store.poller.interval").toNanos,
      TimeUnit.NANOSECONDS
    )
  private val dataStorePollerEnabled: Boolean =
    configModule.typesafeConfig.getBoolean(s"sdes.data-store.poller.enabled")
  private val dataStoreMaxFailureCount = configModule.typesafeConfig.getInt(s"sdes.data-store.queue.maxFailureCount")

  private val dataStoreNotificationRepository = new DataStoreWorkItemRepo(mongoModule.mongoComponent)

  val dataStoreQueueService =
    new DataStoreQueueService(
      sdesModule.sdesService,
      dataStoreNotificationRepository,
      dataStorePollLerLimit,
      dataStoreMaxFailureCount,
      dataStoreRetryAfter
    )

  new DataStoreQueuePollingService(
    akkaModule.actorSystem,
    dataStorePollerInitialDelay,
    dataStorePollerInterval,
    dataStorePollerEnabled,
    dataStoreQueueService
  )
}
