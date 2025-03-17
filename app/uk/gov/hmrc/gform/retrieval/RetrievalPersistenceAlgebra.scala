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

import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.retrieval.AuthRetrievals

trait RetrievalPersistenceAlgebra[F[_]] {
  def find(envelopeId: EnvelopeId): F[Option[AuthRetrievals]]
  def get(envelopeId: EnvelopeId): F[AuthRetrievals]
  def upsert(retrieval: AuthRetrievals): F[Unit]
  def delete(envelopeId: EnvelopeId): F[Unit]
}
