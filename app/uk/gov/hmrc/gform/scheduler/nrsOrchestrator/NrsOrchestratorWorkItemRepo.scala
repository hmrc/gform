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

package uk.gov.hmrc.gform.scheduler.nrsOrchestrator

import cats.implicits.toFunctorOps
import org.bson.types.ObjectId
import org.mongodb.scala.model.Filters.equal
import uk.gov.hmrc.crypto.{ Decrypter, Encrypter }
import uk.gov.hmrc.gform.scheduler.WorkItemRepo
import uk.gov.hmrc.mongo.MongoComponent

import scala.concurrent.{ ExecutionContext, Future }

class NrsOrchestratorWorkItemRepo(mongoComponent: MongoComponent)(implicit
  ec: ExecutionContext,
  jsonCrypto: Encrypter with Decrypter
) extends WorkItemRepo[NrsOrchestratorWorkItem](
      mongoComponent,
      "nrsOrchestratorWorkItem",
      extraIndexes = Seq()
    )(implicitly, NrsOrchestratorWorkItem.formatEncrypted) {
  def delete(id: String): Future[Unit] =
    collection
      .deleteOne(equal("_id", new ObjectId(id)))
      .toFuture()
      .map(_.getDeletedCount > 0)
      .void
}
