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

import akka.actor.{ Actor, ActorLogging }
import uk.gov.hmrc.gform.scheduler.quartz.QSchedulingActor.QScheduledMessage
import uk.gov.hmrc.gform.sdes.alert.SdesAlertService
import uk.gov.hmrc.gform.sdes.renotify.SdesReNotifyService

import scala.concurrent.ExecutionContext
class QSchedulingActor(implicit ec: ExecutionContext) extends Actor with ActorLogging {
  override def receive: Receive = { case message: QScheduledMessage[_] =>
    message.service.invoke(ec) onComplete {
      case scala.util.Success(_)  => log.info(s"Successfully executed ${message.service}")
      case scala.util.Failure(ex) => log.error(s"Failed to execute ${message.service}", ex)
    }
  }
}
object QSchedulingActor {
  sealed trait QScheduledMessage[A] {
    val service: QScheduledService[A]
  }
  case class SdesAlert(service: SdesAlertService) extends QScheduledMessage[Unit] {}
  case class SdesReNotify(service: SdesReNotifyService) extends QScheduledMessage[Unit] {}
}
