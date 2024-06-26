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

import play.api.inject.ApplicationLifecycle
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.scheduler.datastore.{ DataStoreQueuePollingService, DataStoreQueueService, DataStoreWorkItemRepo }
import uk.gov.hmrc.gform.scheduler.dms.{ DmsQueuePollingService, DmsQueueService, DmsWorkItemRepo }
import uk.gov.hmrc.gform.scheduler.quartz.jobs.{ SdesRenotifyJob, SdesSubmissionAlertJob, SdesWorkItemAlertJob }
import uk.gov.hmrc.gform.sdes.SdesModule

import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

class SchedulerModule(
  configModule: ConfigModule,
  mongoModule: MongoModule,
  sdesModule: SdesModule,
  akkaModule: AkkaModule,
  applicationLifecycle: ApplicationLifecycle
)(implicit ex: ExecutionContext) {

  private val dmsRetryAfter: Long = configModule.configuration.getMillis("work-item.sdes.dms.queue.retryAfter")
  private val dmsPollLerLimit = configModule.typesafeConfig.getInt("work-item.sdes.dms.poller.pollLimit")
  private val dmsPollerInitialDelay: FiniteDuration = FiniteDuration(
    configModule.typesafeConfig.getDuration("work-item.sdes.dms.poller.initialDelay").toNanos,
    TimeUnit.NANOSECONDS
  )
  private val dmsPollerInterval: FiniteDuration =
    FiniteDuration(
      configModule.typesafeConfig.getDuration("work-item.sdes.dms.poller.interval").toNanos,
      TimeUnit.NANOSECONDS
    )
  private val dmsPollerEnabled: Boolean = configModule.typesafeConfig.getBoolean("work-item.sdes.dms.poller.enabled")
  private val dmsMaxFailureCount = configModule.typesafeConfig.getInt("work-item.sdes.dms.queue.maxFailureCount")

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

  private val dataStoreRetryAfter: Long =
    configModule.configuration.getMillis("work-item.sdes.data-store.queue.retryAfter")
  private val dataStorePollLerLimit = configModule.typesafeConfig.getInt("work-item.sdes.data-store.poller.pollLimit")
  private val dataStorePollerInitialDelay: FiniteDuration =
    FiniteDuration(
      configModule.typesafeConfig.getDuration("work-item.sdes.data-store.poller.initialDelay").toNanos,
      TimeUnit.NANOSECONDS
    )
  private val dataStorePollerInterval: FiniteDuration =
    FiniteDuration(
      configModule.typesafeConfig.getDuration("work-item.sdes.data-store.poller.interval").toNanos,
      TimeUnit.NANOSECONDS
    )
  private val dataStorePollerEnabled: Boolean =
    configModule.typesafeConfig.getBoolean("work-item.sdes.data-store.poller.enabled")
  private val dataStoreMaxFailureCount =
    configModule.typesafeConfig.getInt("work-item.sdes.data-store.queue.maxFailureCount")

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

  new SdesSubmissionAlertJob(
    sdesModule.sdesSubmissionAlertService,
    applicationLifecycle,
    akkaModule.actorSystem,
    configModule.sdesAlertConfig.enabled,
    configModule.sdesAlertConfig.cron
  )

  new SdesWorkItemAlertJob(
    sdesModule.sdesWorkItemAlertService,
    applicationLifecycle,
    akkaModule.actorSystem,
    configModule.workItemAlertConfig.enabled,
    configModule.workItemAlertConfig.cron
  )

  new SdesRenotifyJob(
    sdesModule.sdesRenotifyQScheduledService,
    applicationLifecycle,
    akkaModule.actorSystem,
    configModule.sdesRenotifyConfig.enabled,
    configModule.sdesRenotifyConfig.cron
  )
}
