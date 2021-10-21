/*
 * Copyright 2021 HM Revenue & Customs
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

import akka.http.scaladsl.model.StatusCodes
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.http.UpstreamErrorResponse

import scala.concurrent.{ ExecutionContext, Future }

trait FormRedirectAlgebra[F[_]] {
  def get(id: FormTemplateId): F[FormRedirect]
}

class FormRedirectService(formRedirectRepo: Repo[FormRedirect])(implicit
  ec: ExecutionContext
) extends FormRedirectAlgebra[Future] {

  override def get(id: FormTemplateId): Future[FormRedirect] =
    formRedirectRepo.find(id.value) map {
      case None =>
        throw UpstreamErrorResponse(
          s"Not found 'formRedirect' for the given id: '${id.value}'",
          StatusCodes.NotFound.intValue
        )
      case Some(formRedirect) =>
        formRedirect
    }
}
