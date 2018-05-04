/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform

import cats.data.EitherT
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.exceptions.UnexpectedState

package object core {

  type FOpt[A] = EitherT[Future, UnexpectedState, A]
  type Opt[A] = Either[UnexpectedState, A]

  def success[A](a: A): FOpt[A] =
    EitherT[Future, UnexpectedState, A](Future.successful(Right(a)))

  def fromFutureOptA[A](fa: Future[Opt[A]]): FOpt[A] =
    EitherT[Future, UnexpectedState, A](fa)

  def fromFutureA[A](fa: Future[A])(implicit ec: ExecutionContext): FOpt[A] =
    EitherT[Future, UnexpectedState, A](fa.map(Right(_)))

  def fromOptA[A](oa: Opt[A]): FOpt[A] =
    EitherT[Future, UnexpectedState, A](Future.successful(oa))

  def fromFutureOptionA[A](fo: Future[Option[A]])(invalid: => UnexpectedState)(
    implicit ec: ExecutionContext): FOpt[A] = {
    val futureA = fo.map {
      case Some(a) => Right(a)
      case None    => Left(invalid)
    }
    EitherT[Future, UnexpectedState, A](futureA)
  }
}
