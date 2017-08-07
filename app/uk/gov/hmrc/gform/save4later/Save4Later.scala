/*
 * Copyright 2017 HM Revenue & Customs
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

import play.api.Logger
import play.api.libs.json.{ JsValue, Json }
import uk.gov.hmrc.gform.core.{ FOpt, Opt }
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId }
import uk.gov.hmrc.http.cache.client.{ CacheMap, ShortLivedCache }
import uk.gov.hmrc.play.http.{ HeaderCarrier, NotFoundException }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ ExecutionContext, Future }

class Save4Later(cache: ShortLivedCache, ex: ExecutionContext) {

  def find(formId: FormId)(implicit hc: HeaderCarrier): Future[Option[Form]] =
    cache.fetchAndGetEntry[Form](formId.value, formCacheKey)

  def get(formId: FormId)(implicit hc: HeaderCarrier): Future[Form] =
    find(formId) map {
      //use the same API as using WSHTTP
      case None => throw new NotFoundException(s"Not found 'form' for the given id: '${formId.value}'")
      case Some(form) => form
    }

  def upsert(formId: FormId, form: Form)(implicit hc: HeaderCarrier): Future[Unit] = {
    cache.cache[Form](formId.value, formCacheKey, form).map(_ => ())
  }

  def delete(formId: FormId)(implicit hc: HeaderCarrier): Future[Unit] = {
    cache.remove(formId.value).map(_ => ())
  }

  def saveKeyStore(formId: FormId, data: Map[String, JsValue])(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Unit] = {
    cache.cache[Map[String, JsValue]](formId.value, "keystore", data).map(_ => ())
  }

  def getKeyStore(formId: FormId)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Option[Map[String, JsValue]]] =
    cache.fetchAndGetEntry[Map[String, JsValue]](formId.value, "keystore")

  private lazy val formCacheKey = "form"
}
