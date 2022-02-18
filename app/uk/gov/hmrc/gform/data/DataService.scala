/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.data

import org.mongodb.scala.model.Filters.{ and, equal, notEqual, regex }
import uk.gov.hmrc.gform.formtemplate.FormTemplateService
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, SavedForm, Submitted }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, NotPermitted }

import scala.concurrent.{ ExecutionContext, Future }

trait DataAlgebra[F[_]] {
  def getSavedFormCount(formTemplateId: FormTemplateId): F[SavedForm]
}

class DataService(formRepo: Repo[Form], formTemplateService: FormTemplateService)(implicit ec: ExecutionContext)
    extends DataAlgebra[Future] {

  override def getSavedFormCount(formTemplateId: FormTemplateId): Future[SavedForm] = {
    val baseQuery =
      and(equal("data.form.formTemplateId", formTemplateId.value), notEqual("data.form.status", Submitted.toString))

    val queryOfEmail = and(baseQuery, regex("data.form.userId", "^email"))

    val queryOfGG = and(
      baseQuery,
      and(regex("data.form.userId", "^(?!email).*"), regex("data.form.userId", "^(?!anonymous-session).*"))
    )

    for {
      formTemplate <- formTemplateService.get(formTemplateId)
      res <- formTemplate.draftRetrievalMethod match {
               case NotPermitted => Future.successful(SavedForm(formTemplateId, 0, 0))
               case _ =>
                 for {
                   countOfEmail <- formRepo.count(queryOfEmail)
                   countOfGG    <- formRepo.count(queryOfGG)
                 } yield SavedForm(formTemplateId, countOfEmail, countOfGG)
             }
    } yield res
  }
}
