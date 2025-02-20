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

package uk.gov.hmrc.gform.save4later

import org.apache.pekko.http.scaladsl.model.StatusCodes
import org.mongodb.scala.model.{ Filters, Updates }
import play.api.libs.json.Format
import uk.gov.hmrc.crypto.{ Decrypter, Encrypter }
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId, FormIdData, Submitted }
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.http.{ HeaderCarrier, UpstreamErrorResponse }
import uk.gov.hmrc.mongo.cache.{ DataKey, MongoCacheRepository }

import scala.concurrent.{ ExecutionContext, Future }

class FormMongoCache(
  mongoCacheRepository: MongoCacheRepository[String],
  jsonCrypto: Encrypter with Decrypter,
  timeProvider: TimeProvider
)(implicit
  ec: ExecutionContext
) extends FormPersistenceAlgebra[Future] {

  implicit val formatFormEncrypted: Format[Form] = EncryptedFormFormat.formatEncrypted(jsonCrypto)

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

  private def findLegacyForm(formId: FormId): Future[Option[Form]] = {
    implicit val formatFormEncrypted: Format[Form] = EncyryptedFormat.formatEncrypted[Form](jsonCrypto)(Form.format)
    mongoCacheRepository
      .get[Form](formId.value)(formDataKey)
  }

  private def getLegacyForm(formId: FormId): Future[Form] = findLegacyForm(formId) map {
    case None =>
      throw UpstreamErrorResponse(
        s"Not found 'legacy-form' for the given id: '${formId.value}'",
        StatusCodes.NotFound.intValue
      )
    case Some(form) =>
      form
  }

  override def get(formIdData: FormIdData)(implicit hc: HeaderCarrier): Future[Form] = get(formIdData.toFormId)
    .recoverWith {
      case UpstreamErrorResponse.WithStatusCode(statusCode) if statusCode == StatusCodes.NotFound.intValue =>
        getLegacyForm(formIdData.toFormId)
    }

  override def upsert(form: Form)(implicit hc: HeaderCarrier): Future[Unit] =
    mongoCacheRepository
      .put(form._id.value)(formDataKey, form)
      .andThen {
        case _ if form.status == Submitted =>
          mongoCacheRepository.collection
            .findOneAndUpdate(
              filter = Filters.equal("_id", form._id.value),
              update = Updates.set("submitDetails.createdAt", timeProvider.instant())
            )
            .toFuture()
      }
      .map(_ => ())

  override def delete(formId: FormId)(implicit hc: HeaderCarrier): Future[Unit] =
    mongoCacheRepository.deleteEntity(formId.value)
}
