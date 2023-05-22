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

package uk.gov.hmrc.gform.formredirect

import org.mongodb.scala.model.Filters.equal
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

import scala.concurrent.{ ExecutionContext, Future }

trait FormRedirectAlgebra[F[_]] {
  def find(id: FormTemplateId): F[Option[FormRedirect]]

  def findAllPaginated(page: Int, pageSize: Int): F[FormRedirectPageData]
}

class FormRedirectService(formRedirectRepo: Repo[FormRedirect])(implicit ec: ExecutionContext)
    extends FormRedirectAlgebra[Future] {

  override def find(id: FormTemplateId): Future[Option[FormRedirect]] =
    formRedirectRepo.find(id.value)

  override def findAllPaginated(page: Int, pageSize: Int): Future[FormRedirectPageData] = {
    val sort = equal("redirect", -1)
    val skip = page * pageSize
    for {
      formRedirects <- formRedirectRepo.collection
                         .find()
                         .sort(sort)
                         .skip(skip)
                         .limit(pageSize)
                         .toFuture()
                         .map(_.toList)
      count <- formRedirectRepo.countAll()
    } yield FormRedirectPageData(formRedirects, count)
  }
}
