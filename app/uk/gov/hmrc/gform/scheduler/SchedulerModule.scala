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
import pureconfig.ConfigSource
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.scheduler.datastore.{ DataStoreQueuePollingService, DataStoreQueueService, DataStoreWorkItemRepo }
import uk.gov.hmrc.gform.scheduler.dms.{ DmsQueuePollingService, DmsQueueService, DmsWorkItemRepo }
import uk.gov.hmrc.gform.scheduler.infoarchive.{ InfoArchiveQueuePollingService, InfoArchiveQueueService, InfoArchiveWorkItemRepo }
import uk.gov.hmrc.gform.scheduler.quartz.jobs.{ SdesRenotifyJob, SdesSubmissionAlertJob, SdesWorkItemAlertJob }
import uk.gov.hmrc.gform.sdes.SdesModule

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import pureconfig.generic.auto._

class SchedulerModule(
  configModule: ConfigModule,
  mongoModule: MongoModule,
  sdesModule: SdesModule,
  akkaModule: AkkaModule,
  applicationLifecycle: ApplicationLifecycle
)(implicit ex: ExecutionContext) {

  case class SdesConfig(dms: DestinationConfig, dataStore: DestinationConfig, infoArchive: DestinationConfig)
  case class DestinationConfig(queue: QueueConfig, poller: PollerConfig)
  case class QueueConfig(retryAfter: FiniteDuration, maxFailureCount: Int)
  case class PollerConfig(enabled: Boolean, initialDelay: FiniteDuration, interval: FiniteDuration, pollLimit: Int)

  private val config = ConfigSource.default.at("work-item.sdes").loadOrThrow[SdesConfig]

  private val dmsNotificationRepository = new DmsWorkItemRepo(mongoModule.mongoComponent)

  val dmsQueueService =
    new DmsQueueService(
      sdesModule.sdesService,
      dmsNotificationRepository,
      config.dms.poller.pollLimit,
      config.dms.queue.maxFailureCount,
      config.dms.queue.retryAfter.toMillis
    )

  new DmsQueuePollingService(
    akkaModule.actorSystem,
    config.dms.poller.initialDelay,
    config.dms.poller.interval,
    config.dms.poller.enabled,
    dmsQueueService
  )

  private val dataStoreNotificationRepository = new DataStoreWorkItemRepo(mongoModule.mongoComponent)

  val dataStoreQueueService =
    new DataStoreQueueService(
      sdesModule.sdesService,
      dataStoreNotificationRepository,
      config.dataStore.poller.pollLimit,
      config.dataStore.queue.maxFailureCount,
      config.dataStore.queue.retryAfter.toMillis
    )

  new DataStoreQueuePollingService(
    akkaModule.actorSystem,
    config.dataStore.poller.initialDelay,
    config.dataStore.poller.interval,
    config.dataStore.poller.enabled,
    dataStoreQueueService
  )

  private val infoArchiveNotificationRepository = new InfoArchiveWorkItemRepo(mongoModule.mongoComponent)

  val infoArchiveQueueService =
    new InfoArchiveQueueService(
      sdesModule.sdesService,
      infoArchiveNotificationRepository,
      config.infoArchive.poller.pollLimit,
      config.infoArchive.queue.maxFailureCount,
      config.infoArchive.queue.retryAfter.toMillis
    )

  new InfoArchiveQueuePollingService(
    akkaModule.actorSystem,
    config.infoArchive.poller.initialDelay,
    config.infoArchive.poller.interval,
    config.infoArchive.poller.enabled,
    infoArchiveQueueService
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
