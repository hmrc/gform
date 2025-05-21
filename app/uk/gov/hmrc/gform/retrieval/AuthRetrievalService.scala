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

import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.retrieval.{ AuthRetrievals, AuthRetrievalsByFormIdData }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class AuthRetrievalService(
  retrievalRepo: RetrievalPersistenceAlgebra[Future],
  formService: FormAlgebra[Future]
)(implicit
  ec: ExecutionContext
) {

  def find(envelopeId: EnvelopeId): Future[Option[AuthRetrievals]] = retrievalRepo.find(envelopeId)

  def get(envelopeId: EnvelopeId): Future[AuthRetrievals] = retrievalRepo.get(envelopeId)

  def upsert(retrieval: AuthRetrievals): Future[Unit] = retrievalRepo.upsert(retrieval)

  def upsertByFormIdData(retrieval: AuthRetrievalsByFormIdData)(implicit hc: HeaderCarrier): Future[Unit] =
    for {
      form <- formService.get(retrieval.formIdData)
      retrievalUpd = AuthRetrievals(form.envelopeId, retrieval.materialisedRetrievals)
      _ <- retrievalRepo.upsert(retrievalUpd)
    } yield ()

  def delete(envelopeId: EnvelopeId): Future[Unit] = retrievalRepo.delete(envelopeId)
}
