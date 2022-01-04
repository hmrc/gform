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

package uk.gov.hmrc.gform.save4later

import akka.http.scaladsl.model.StatusCodes
import play.api.libs.json.Format
import uk.gov.hmrc.crypto.CryptoWithKeysFromConfig
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId, FormIdData }
import uk.gov.hmrc.http.{ HeaderCarrier, UpstreamErrorResponse }
import uk.gov.hmrc.mongo.cache.{ DataKey, MongoCacheRepository }

import scala.concurrent.{ ExecutionContext, Future }

class FormMongoCache(mongoCacheRepository: MongoCacheRepository[String], jsonCrypto: CryptoWithKeysFromConfig)(implicit
  ec: ExecutionContext
) extends FormPersistenceAlgebra[Future] {

  implicit val formatFormEncrypted: Format[Form] = EncyryptedFormat.formatEncrypted[Form](jsonCrypto)(Form.format)

  private val formDataKey: DataKey[Form] = DataKey("form")

  override def find(formId: FormId)(implicit hc: HeaderCarrier): Future[Option[Form]] = mongoCacheRepository
    .get[Form](formId.value)(formDataKey)

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

  override def upsert(form: Form)(implicit hc: HeaderCarrier): Future[Unit] =
    mongoCacheRepository
      .put(form._id.value)(formDataKey, form)
      .map(_ => ())

  override def delete(formId: FormId)(implicit hc: HeaderCarrier): Future[Unit] =
    mongoCacheRepository.deleteEntity(formId.value)
}
