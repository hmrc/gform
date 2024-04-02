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

package uk.gov.hmrc.gform.handlebarstemplate

import cats.syntax.functor._
import cats.instances.future._
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.repo.{ DeleteResult, Repo }
import uk.gov.hmrc.gform.sharedmodel.{ HandlebarsSchema, HandlebarsSchemaId }

import scala.concurrent.{ ExecutionContext, Future }

trait HandlebarsSchemaAlgebra[F[_]] {
  def save(handlebarsSchema: HandlebarsSchema): F[Unit]

  def get(handlebarsSchemaId: HandlebarsSchemaId): F[Option[HandlebarsSchema]]

  def delete(handlebarsSchemaId: HandlebarsSchemaId): F[DeleteResult]

  def getAllIds: F[List[HandlebarsSchemaId]]
}

class HandlebarsSchemaService(repo: Repo[HandlebarsSchema])(implicit ec: ExecutionContext)
    extends HandlebarsSchemaAlgebra[Future] {
  private val logger = LoggerFactory.getLogger(getClass)
  override def save(handlebarsSchema: HandlebarsSchema): Future[Unit] = repo
    .replace(handlebarsSchema)
    .toFuture
    .as(logger.info(s"HandlebarsSchema.save(${handlebarsSchema._id.value}) - upserting $handlebarsSchema)"))

  override def get(handlebarsSchemaId: HandlebarsSchemaId): Future[Option[HandlebarsSchema]] =
    repo.find(handlebarsSchemaId.value)

  override def delete(handlebarsSchemaId: HandlebarsSchemaId): Future[DeleteResult] =
    repo.delete(handlebarsSchemaId.value).toFuture

  override def getAllIds: Future[List[HandlebarsSchemaId]] = repo.findAll().map(_.map(_._id))
}
