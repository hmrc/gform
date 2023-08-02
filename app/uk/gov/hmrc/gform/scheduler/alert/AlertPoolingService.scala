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

package uk.gov.hmrc.gform.scheduler.alert

import akka.actor.{ ActorSystem, Cancellable }
import org.slf4j.{ Logger, LoggerFactory }
import uk.gov.hmrc.gform.scheduler.ExclusiveScheduledJob

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success }

abstract class AlertPoolingService[P](
  actorSystem: ActorSystem,
  alertService: AlertAlgebra,
  pollerEnabled: Boolean
)(implicit
  ec: ExecutionContext
) extends ExclusiveScheduledJob {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  logger.debug(s"Starting $name, Initial delay: $initialDelay, Polling interval: $interval")

  callExecutor(name)

  override def executeInMutex(implicit ec: ExecutionContext): Future[Result] =
    alertService.execute.map(_ => Result(s"$name processed"))

  def callExecutor(name: String)(implicit ec: ExecutionContext): Cancellable =
    actorSystem.scheduler.scheduleWithFixedDelay(initialDelay, interval) { () =>
      if (pollerEnabled) {
        executor(name)
      } else {
        logger.warn(s"$name: Poller enabled is false")
      }
    }

  def executor(name: String)(implicit ec: ExecutionContext): Unit =
    execute.onComplete({
      case Success(Result(res)) =>
        logger.info(res)
      case Failure(throwable) =>
        logger.error(s"$name: Exception completing", throwable)
    })
}
