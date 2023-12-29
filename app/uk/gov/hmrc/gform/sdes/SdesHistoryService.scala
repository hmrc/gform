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

package uk.gov.hmrc.gform.sdes

import cats.instances.future._
import cats.syntax.functor._
import org.mongodb.scala.model.Filters
import uk.gov.hmrc.gform.core._
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.sdes.{ CorrelationId, SdesHistory }

import scala.concurrent.{ ExecutionContext, Future }

trait SdesHistoryAlgebra[F[_]] {
  def save(sdesHistory: SdesHistory): F[Unit]
  def get(correlationId: CorrelationId): F[List[SdesHistory]]
}

class SdesHistoryService(repo: Repo[SdesHistory])(implicit ec: ExecutionContext) extends SdesHistoryAlgebra[Future] {
  private val logger = LoggerFactory.getLogger(getClass)
  override def save(sdesHistory: SdesHistory): Future[Unit] =
    repo
      .upsert(sdesHistory)
      .toFuture
      .as(logger.info(s"SdesHistoryService.save(${sdesHistory.correlationId.value}) - upserting"))

  override def get(correlationId: CorrelationId): Future[List[SdesHistory]] =
    repo.search(Filters.equal("correlationId", correlationId.value))
}
