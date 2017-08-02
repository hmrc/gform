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

package uk.gov.hmrc.gform.connectors

import play.api.Logger
import play.api.libs.json.{ JsError, JsSuccess, Json }
import uk.gov.hmrc.gform.WSHttp
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.InvalidState
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.http.cache.client.ShortLivedCache
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class Save4LaterConnector(cache: ShortLivedCache) extends ServicesConfig {

  lazy val http = WSHttp

  def findOne(formId: FormId)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Option[Form]] =
    cache.fetchAndGetEntry[Form](formId.value, "form")

  def find(formId: FormId)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[List[Form]] = {
    cache.fetch(formId.value).map {
      case Some(x) =>
        x.data.values.flatMap { json =>
          Json.fromJson[Form](json) match {
            case JsSuccess(y, _) => List(y)
            case JsError(err) => List.empty[Form]
          }
        }.toList
      case None => List.empty[Form]
    }
  }

  def put(formId: FormId, form: Form)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Opt[DbOperationResult]] = {
    Logger.debug(Json.prettyPrint(Json.toJson(form)) + " Put of form")
    findOne(formId).flatMap {
      case None => save(formId, form)
      case Some(x) =>
        val fields = x.formData.fields ++ form.formData.fields
        val concatenatedFormData: FormData = form.formData.copy(fields = fields)
        save(formId, form.copy(formData = concatenatedFormData))
    }
  }

  def save(formId: FormId, form: Form)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Opt[DbOperationResult]] = {
    cache.cache[Form](formId.value, "form", form)
      .map(_ => Right(Success)).recover {
        case t: Throwable => Left(InvalidState("put Failed"))
      }
  }

  def delete(formId: FormId)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Opt[DbOperationResult]] = {
    cache.remove(formId.value)
      .map(_ => Right(Success))
      .recover {
        case e: Throwable => Left(InvalidState("Delete Failed"))
      }
  }
}
