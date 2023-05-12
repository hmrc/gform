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

package uk.gov.hmrc.gform.handlebarspayload

import cats.syntax.functor._
import cats.instances.future._
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.{ HandlebarsPayload, HandlebarsPayloadId }

import scala.concurrent.{ ExecutionContext, Future }

trait HandlebarsPayloadAlgebra[F[_]] {
  def save(handlebarsPayload: HandlebarsPayload): F[Unit]

  def get(handlebarsPayloadName: HandlebarsPayloadId): F[HandlebarsPayload]
}

class HandlebarsPayloadService(repo: Repo[HandlebarsPayload])(implicit ec: ExecutionContext)
    extends HandlebarsPayloadAlgebra[Future] {
  private val logger = LoggerFactory.getLogger(getClass)
  override def save(handlebarsPayload: HandlebarsPayload): Future[Unit] = repo
    .replace(handlebarsPayload)
    .toFuture
    .as(logger.info(s"HandlebarPayload.save(${handlebarsPayload._id.value}) - upserting $handlebarsPayload)"))

  override def get(handlebarsPayloadId: HandlebarsPayloadId): Future[HandlebarsPayload] =
    repo
      .find(handlebarsPayloadId.value)
      .map(
        _.getOrElse(
          throw new NoSuchElementException(s"`payloadName` is not valid. ${handlebarsPayloadId.value} not found")
        )
      )
}
