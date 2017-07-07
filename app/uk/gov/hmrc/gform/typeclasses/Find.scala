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

package uk.gov.hmrc.gform.typeclasses

import play.api.libs.json.{ JsObject, Json }
import sun.security.krb5.internal.AuthContext
import uk.gov.hmrc.gform.connectors.Save4LaterConnector

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.models.{ Form, FormKey, UserId }
import uk.gov.hmrc.gform.repositories.{ AbstractRepo, FormRepository }
import uk.gov.hmrc.gform.services.IsEncrypt
import uk.gov.hmrc.http.cache.client.ShortLivedCache
import uk.gov.hmrc.play.http.HeaderCarrier

trait Find[T] {
  def apply(selector: JsObject, projection: JsObject = Json.obj()): Future[List[T]]
}

object Find {
  implicit def form(implicit repo: AbstractRepo[Form], cache: Save4LaterConnector, ex: ExecutionContext, hc: HeaderCarrier) = new Find[Form] {
    def apply(selector: JsObject, projection: JsObject): Future[List[Form]] = {
      if (IsEncrypt.is.value) {
        selector.asOpt[FormKey] match {
          case Some(x) => cache.find(x)
          case None => Future.successful(List.empty[Form])
        }
      } else {
        repo.find(selector, projection)
      }
    }
  }
}
