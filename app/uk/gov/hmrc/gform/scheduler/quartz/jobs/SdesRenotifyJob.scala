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

package uk.gov.hmrc.gform.scheduler.quartz.jobs

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import com.typesafe.akka.extension.quartz.QuartzSchedulerExtension
import play.api.inject.ApplicationLifecycle
import uk.gov.hmrc.gform.scheduler.quartz.QScheduledJob
import uk.gov.hmrc.gform.scheduler.quartz.QSchedulingActor
import uk.gov.hmrc.gform.scheduler.quartz.QSchedulingActor.SdesRenotify
import uk.gov.hmrc.gform.sdes.renotify.SdesRenotifyQScheduledService

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class SdesRenotifyJob(
  val sdesRenotifyQScheduledService: SdesRenotifyQScheduledService,
  val applicationLifecycle: ApplicationLifecycle,
  val jobActorSystem: ActorSystem,
  val jobEnabled: Boolean,
  val jobExpression: String
)(implicit ex: ExecutionContext)
    extends QScheduledJob {

  override val jobName: String = "SdesRenotifyJob"
  override val scheduler: QuartzSchedulerExtension = QuartzSchedulerExtension(jobActorSystem)
  override val schedulingActorRef: ActorRef = jobActorSystem.actorOf(Props(new QSchedulingActor()))
  override val scheduledMessage: SdesRenotify = SdesRenotify(sdesRenotifyQScheduledService)
  override val enabled: Boolean = jobEnabled
  override val expression: String = jobExpression
  override val description: Option[String] = Some(
    "It renotifies the submissions in FileReady state"
  )

  schedule()

  applicationLifecycle.addStopHook { () =>
    Future.successful(scheduler.cancelJob(jobName))
    Future.successful(scheduler.shutdown(waitForJobsToComplete = false))
  }
}