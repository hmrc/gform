/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.it

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{ Level, Logger => LogbackLogger }
import ch.qos.logback.core.read.ListAppender
import play.api.{ Logger, LoggerLike }

import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.reflect.ClassTag

trait LogCapturing {

  def withCaptureOfLoggingFrom(logger: LoggerLike)(body: (=> List[ILoggingEvent]) => Unit): Unit =
    withCaptureOfLoggingFrom(List(logger))(body)

  def withCaptureOfLoggingFrom(loggerName: String)(body: (=> List[ILoggingEvent]) => Unit): Unit =
    withCaptureOfLoggingFrom(List(Logger(loggerName)))(body)

  def withCaptureOfLoggingFrom[T](body: (=> List[ILoggingEvent]) => Unit)(implicit classTag: ClassTag[T]): Unit =
    withCaptureOfLoggingFrom(List(Logger(classTag.runtimeClass)))(body)

  def withCaptureOfLoggingFrom[T, U](
    body: (=> List[ILoggingEvent]) => Unit
  )(implicit classTagT: ClassTag[T], classTagU: ClassTag[U]): Unit =
    withCaptureOfLoggingFrom(List(Logger(classTagT.runtimeClass), Logger(classTagU.runtimeClass)))(body)

  private def withCaptureOfLoggingFrom(loggers: List[LoggerLike])(body: (=> List[ILoggingEvent]) => Unit): Unit = {
    val appender = new ListAppender[ILoggingEvent]()

    loggers.foreach { logger =>
      val underlying = logger.logger.asInstanceOf[LogbackLogger]
      appender.setContext(underlying.getLoggerContext)
      underlying.addAppender(appender)
      underlying.setLevel(Level.ALL)
      underlying.setAdditive(true)
    }

    appender.start()
    body(appender.list.asScala.toList)
  }
}
