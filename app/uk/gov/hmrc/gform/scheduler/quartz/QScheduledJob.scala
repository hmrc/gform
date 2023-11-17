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

package uk.gov.hmrc.gform.scheduler.quartz

import akka.actor.ActorRef
import com.typesafe.akka.extension.quartz.QuartzSchedulerExtension
import play.api.Logging
import uk.gov.hmrc.gform.scheduler.quartz.QSchedulingActor.QScheduledMessage

trait QScheduledJob extends Logging {
  val jobName: String
  val scheduledMessage: QScheduledMessage[_]
  val enabled: Boolean
  val expression: String
  val description: Option[String]
  val scheduler: QuartzSchedulerExtension
  val schedulingActorRef: ActorRef

  def schedule(): Unit =
    (enabled, expression.nonEmpty) match {
      case (true, true) =>
        scheduler.createSchedule(jobName, description, expression)
        scheduler.schedule(jobName, schedulingActorRef, scheduledMessage)
        logger.info(s"Scheduler for $jobName has been started")
      case (true, false) =>
        logger.info(s"Scheduler for $jobName is disabled as there is no quartz expression")
      case (false, _) =>
        logger.info(s"Scheduler for $jobName is disabled by configuration")
    }

}
