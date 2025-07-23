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

package uk.gov.hmrc.gform.log

import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Filters.equal
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.core._

import scala.concurrent.{ ExecutionContext, Future }

trait DataAccessAlgebra[F[_]] {
  def save(log: DataAccessLog): F[Unit]

  def page(page: Int, pageSize: Int): F[List[DataAccessLog]]
}

class DataAccessService(logRepo: Repo[DataAccessLog])(implicit
  ec: ExecutionContext
) extends DataAccessAlgebra[Future] {

  override def save(log: DataAccessLog): Future[Unit] = logRepo.upsert(log).toFuture

  override def page(page: Int, pageSize: Int): Future[List[DataAccessLog]] = {
    val query = Filters.empty()
    val sort = equal("createdAt", -1)
    val skip = page * pageSize
    logRepo.page(query, sort, skip, pageSize)
  }
}
