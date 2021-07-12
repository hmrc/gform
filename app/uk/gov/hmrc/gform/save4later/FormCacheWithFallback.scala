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
import org.slf4j.{ Logger, LoggerFactory }
import play.api.libs.json.Json
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId, FormIdData }
import uk.gov.hmrc.http.{ HeaderCarrier, UpstreamErrorResponse }

import scala.concurrent.{ ExecutionContext, Future }

class FormCacheWithFallback(formMongoCache: FormMongoCache, save4Later: Save4Later)(implicit ec: ExecutionContext)
    extends FormPersistenceAlgebra[Future] {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  override def find(formId: FormId)(implicit hc: HeaderCarrier): Future[Option[Form]] = formMongoCache
    .find(formId)
    .flatMap { maybeForm =>
      maybeForm.fold {
        logger.info(s"Form not found in local mongo cache. Checking save4later [formId=${formId.value}]")
        save4Later.find(formId)
      } { form =>
        Future.successful(Some(form))
      }
    }

  override def get(formId: FormId)(implicit hc: HeaderCarrier): Future[Form] = find(formId) map {
    case None =>
      throw UpstreamErrorResponse(
        s"Not found 'form' for the given id: '${formId.value}'",
        StatusCodes.NotFound.intValue
      )
    case Some(form) =>
      form
  }

  override def get(formIdData: FormIdData)(implicit hc: HeaderCarrier): Future[Form] = get(formIdData.toFormId)

  override def upsert(formId: FormId, form: Form)(implicit hc: HeaderCarrier): Future[Unit] =
    formMongoCache.upsert(formId, form)

  override def delete(formId: FormId)(implicit hc: HeaderCarrier): Future[Unit] =
    formMongoCache.delete(formId).flatMap(_ => save4Later.delete(formId))
}
