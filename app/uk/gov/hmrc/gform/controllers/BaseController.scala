/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.controllers

import java.util.UUID

import cats.data.EitherT
import cats.implicits._
import play.api.libs.json._
import play.api.mvc.Result

import scala.concurrent.{ ExecutionContext, Future }

class BaseController(implicit ec: ExecutionContext) extends uk.gov.hmrc.play.bootstrap.controller.BaseController {

  object O {
    def asOkJson[T: Writes](t: T): Result =
      Ok(Json.toJson(t))
  }

  implicit class FutureOps[T: Writes](f: Future[T]) {
    def asOkJson = f.map(t => O.asOkJson(t))
  }

  implicit class FutureOps2(f: Future[_]) {
    def asNoContent = f.map(_ => NoContent)
  }

  type LeftResult[T] = EitherT[Future, Result, T]

  def asRes[T](fa: Future[T]): LeftResult[T] =
    EitherT[Future, Result, T](fa.map(_.asRight))

  def asRes[E, T](fa: Future[Either[E, T]])(toLeftResult: E => Result): LeftResult[T] =
    EitherT[Future, E, T](fa).leftMap(toLeftResult)

  def asRes[T](a: T): LeftResult[T] = EitherT[Future, Result, T](Future.successful(a.asRight))

}

case class ErrResponse(
  error: String,
  details: Option[JsValue] = None,
  occurrenceId: String = UUID.randomUUID().toString)

object ErrResponse {
  implicit val format: OFormat[ErrResponse] = Json.format[ErrResponse]
}
