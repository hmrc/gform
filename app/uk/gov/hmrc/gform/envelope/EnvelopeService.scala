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

package uk.gov.hmrc.gform.envelope

import cats.syntax.functor._
import cats.instances.future._
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.envelope.EnvelopeData
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId

import scala.concurrent.{ ExecutionContext, Future }

trait EnvelopeAlgebra[F[_]] {
  def save(envelope: EnvelopeData): F[Unit]

  def get(envelopeId: EnvelopeId): F[EnvelopeData]

  def find(envelopeId: EnvelopeId): F[Option[EnvelopeData]]
}

class EnvelopeService(envelopeRepo: Repo[EnvelopeData])(implicit
  ec: ExecutionContext
) extends EnvelopeAlgebra[Future] {
  private val logger = LoggerFactory.getLogger(getClass)

  override def save(envelope: EnvelopeData): Future[Unit] = envelopeRepo
    .upsert(envelope)
    .toFuture
    .as(logger.info(s"EnvelopeAlgebra.save(${envelope._id.value}) - upserting $envelope)"))

  override def get(envelopeId: EnvelopeId): Future[EnvelopeData] = envelopeRepo.get(envelopeId.value)

  override def find(envelopeId: EnvelopeId): Future[Option[EnvelopeData]] = envelopeRepo.find(envelopeId.value)
}
