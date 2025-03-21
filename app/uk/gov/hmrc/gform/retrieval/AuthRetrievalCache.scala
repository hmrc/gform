/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.retrieval

import org.apache.pekko.http.scaladsl.model.StatusCodes
import play.api.libs.json.Format
import uk.gov.hmrc.crypto.{ Decrypter, Encrypter }
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.retrieval.AuthRetrievals
import uk.gov.hmrc.http.UpstreamErrorResponse
import uk.gov.hmrc.mongo.cache.{ DataKey, MongoCacheRepository }

import scala.concurrent.{ ExecutionContext, Future }

class AuthRetrievalCache(
  mongoCacheRepository: MongoCacheRepository[String],
  jsonCrypto: Encrypter with Decrypter
)(implicit
  ec: ExecutionContext
) extends RetrievalPersistenceAlgebra[Future] {
  implicit val formatEncrypted: Format[AuthRetrievals] = EncryptedAuthRetrievalFormat.formatEncrypted(jsonCrypto)

  private val retrievalsDataKey: DataKey[AuthRetrievals] = DataKey("retrievals")

  override def find(envelopeId: EnvelopeId): Future[Option[AuthRetrievals]] = mongoCacheRepository
    .get[AuthRetrievals](envelopeId.value)(retrievalsDataKey)

  override def get(envelopeId: EnvelopeId): Future[AuthRetrievals] = find(envelopeId) map {
    case None =>
      throw UpstreamErrorResponse(
        s"Not found 'authRetrieval' for the given id: '${envelopeId.value}'",
        StatusCodes.NotFound.intValue
      )
    case Some(retrieval) =>
      retrieval
  }

  override def upsert(retrieval: AuthRetrievals): Future[Unit] =
    mongoCacheRepository
      .put(retrieval._id.value)(retrievalsDataKey, retrieval)
      .map(_ => ())

  override def delete(envelopeId: EnvelopeId): Future[Unit] =
    mongoCacheRepository.deleteEntity(envelopeId.value)
}
