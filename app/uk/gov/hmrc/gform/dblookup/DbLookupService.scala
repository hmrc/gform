/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.dblookup

import uk.gov.hmrc.gform.formtemplate.Verifier
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.dblookup.{ CollectionName, DbLookupId }

import scala.concurrent.{ ExecutionContext, Future }

trait DbLookupAlgebra[F[_]] {
  def find(id: DbLookupId, collectionName: CollectionName): F[Option[DbLookupId]]
}

class DbLookupService(mongoModule: MongoModule)(implicit ec: ExecutionContext)
    extends Verifier with DbLookupAlgebra[Future] {

  private def dbLookupRepo(collectionName: CollectionName): Repo[DbLookupId] =
    new Repo[DbLookupId](collectionName.name, mongoModule.mongo, _.id)

  def find(dbLookupId: DbLookupId, collectionName: CollectionName): Future[Option[DbLookupId]] =
    dbLookupRepo(collectionName).find(dbLookupId.id)
}
