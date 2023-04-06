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

import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.duration.FiniteDuration

trait ScheduledJob {
  def name: String
  def execute(implicit ec: ExecutionContext): Future[Result]

  case class Result(message: String)

  def initialDelay: FiniteDuration

  def interval: FiniteDuration

  override def toString() = s"$name after $initialDelay every $interval"
}
