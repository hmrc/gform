/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.scheduler.history
import cats.implicits.toFunctorOps
import uk.gov.hmrc.gform.core._
import org.mongodb.scala.model.Filters
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId

import scala.concurrent.{ ExecutionContext, Future }

trait WorkItemHistoryAlgebra[F[_]] {
  def save(workItemHistory: WorkItemHistory): F[Unit]
  def find(envelopeId: EnvelopeId, destinationId: DestinationId): F[List[WorkItemHistory]]
}

class WorkItemHistoryService(repo: Repo[WorkItemHistory])(implicit ec: ExecutionContext)
    extends WorkItemHistoryAlgebra[Future] {
  private val logger = LoggerFactory.getLogger(getClass)
  override def save(workItemHistory: WorkItemHistory): Future[Unit] =
    repo
      .upsert(workItemHistory)
      .toFuture
      .as(logger.info(s"ApiHistoryService.save(${workItemHistory.envelopeId.value}) - upserting"))

  override def find(envelopeId: EnvelopeId, destinationId: DestinationId): Future[List[WorkItemHistory]] =
    repo.search(
      Filters.and(Filters.equal("envelopeId", envelopeId.value), Filters.equal("destinationId", destinationId.id))
    )

}
