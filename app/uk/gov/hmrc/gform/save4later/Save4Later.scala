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

package uk.gov.hmrc.gform.save4later

import akka.http.scaladsl.model.StatusCodes
import cats.instances.future._
import cats.syntax.functor._
import org.slf4j.LoggerFactory
import play.api.libs.json.Json
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId, FormIdData }
import uk.gov.hmrc.http.cache.client.ShortLivedCache

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.{ HeaderCarrier, UpstreamErrorResponse }

class Save4Later(cache: ShortLivedCache)(implicit ex: ExecutionContext) extends FormPersistenceAlgebra[Future] {

  def find(formId: FormId)(implicit hc: HeaderCarrier): Future[Option[Form]] =
    cache.fetchAndGetEntry[Form](formId.value, formCacheKey)

  def get(formId: FormId)(implicit hc: HeaderCarrier): Future[Form] =
    find(formId) map {
      //use the same API as using WSHTTP
      case None =>
        throw UpstreamErrorResponse(
          s"Not found 'form' for the given id: '${formId.value}'",
          StatusCodes.NotFound.intValue
        )
      case Some(form) =>
        LoggerFactory.getLogger(getClass.getName).debug(Json.prettyPrint(Json.toJson(form)) + "GETFORM")
        form
    }

  def get(formIdData: FormIdData)(implicit hc: HeaderCarrier): Future[Form] = get(formIdData.toFormId)

  def upsert(form: Form)(implicit hc: HeaderCarrier): Future[Unit] = {
    LoggerFactory.getLogger(getClass.getName).debug(Json.prettyPrint(Json.toJson(form)) + "PUTFORM")
    cache.cache[Form](form._id.value, formCacheKey, form).void
  }

  def delete(formId: FormId)(implicit hc: HeaderCarrier): Future[Unit] =
    cache.remove(formId.value).void

  private lazy val formCacheKey = "form"
}
