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
import uk.gov.hmrc.gform.sharedmodel.{ HandlebarsTemplate, HandlebarsTemplateId }

import scala.concurrent.{ ExecutionContext, Future }

trait HandlebarsTemplateAlgebra[F[_]] {
  def save(handlebarsTemplate: HandlebarsTemplate): F[Unit]

  def get(handlebarsTemplateId: HandlebarsTemplateId): F[Option[HandlebarsTemplate]]

  def delete(handlebarsTemplateId: HandlebarsTemplateId): F[DeleteResult]

  def getAll: F[List[HandlebarsTemplateId]]
}

class HandlebarsTemplateService(repo: Repo[HandlebarsTemplate])(implicit ec: ExecutionContext)
    extends HandlebarsTemplateAlgebra[Future] {
  private val logger = LoggerFactory.getLogger(getClass)
  override def save(handlebarsTemplate: HandlebarsTemplate): Future[Unit] = repo
    .replace(handlebarsTemplate)
    .toFuture
    .as(logger.info(s"HandlebarTemplate.save(${handlebarsTemplate._id.value}) - upserting $handlebarsTemplate)"))

  override def get(handlebarsTemplateId: HandlebarsTemplateId): Future[Option[HandlebarsTemplate]] =
    repo.find(handlebarsTemplateId.value)

  override def delete(handlebarsTemplateId: HandlebarsTemplateId): Future[DeleteResult] =
    repo.delete(handlebarsTemplateId.value).toFuture

  override def getAll: Future[List[HandlebarsTemplateId]] = repo.findAll().map(_.map(_._id))
}
