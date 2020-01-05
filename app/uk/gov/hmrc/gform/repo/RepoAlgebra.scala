/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.repo

import play.api.libs.json._
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }

import scala.concurrent.ExecutionContext

trait RepoAlgebra[T, F[_]] {
  def upsert(t: T): F[Unit]
  def get(id: String): F[T]
  def search(selector: JsObject): F[List[T]]
  def search(selector: JsObject, orderBy: JsObject): F[List[T]]
}

object RepoAlgebra {
  def fOpt[T: OWrites: Manifest](repo: Repo[T])(implicit ec: ExecutionContext): RepoAlgebra[T, FOpt] =
    new RepoAlgebra[T, FOpt] {
      override def upsert(t: T): FOpt[Unit] = repo.upsert(t)
      override def get(id: String): FOpt[T] = fromFutureA(repo.get(id))
      override def search(selector: JsObject): FOpt[List[T]] = fromFutureA(repo.search(selector))
      override def search(selector: JsObject, orderBy: JsObject): FOpt[List[T]] =
        fromFutureA(repo.search(selector, orderBy))
    }
}
