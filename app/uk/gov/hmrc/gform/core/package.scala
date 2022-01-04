/*
 * Copyright 2022 HM Revenue & Customs
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

import cats.{ MonadError, StackSafeMonad }
import cats.data.EitherT
import cats.implicits.catsStdInstancesForFuture

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

  def fromFutureOptionA[A](fo: Future[Option[A]])(invalid: => UnexpectedState)(implicit ec: ExecutionContext): FOpt[A] =
    EitherT[Future, UnexpectedState, A](fo.map(_.toRight(invalid)))

  implicit class FutureSyntax[T](f: Future[T]) {
    def void(implicit ec: ExecutionContext): Future[Unit] = f.map(_ => ())
  }

  implicit class FOptToFuture[A](opt: FOpt[A]) {
    def toFuture(implicit ex: ExecutionContext): Future[A] = opt.value flatMap {
      case Right(res) => Future.successful(res)
      case Left(e)    => Future.failed(new Exception(e.error))
    }
  }

  implicit def fOptMonadError(implicit ec: ExecutionContext): MonadError[FOpt, String] =
    new MonadError[FOpt, String] with StackSafeMonad[FOpt] {
      override def flatMap[A, B](fa: FOpt[A])(f: A => FOpt[B]): FOpt[B] = fa.flatMap(f)
      override def pure[A](x: A): FOpt[A] = success(x)
      override def raiseError[A](e: String): FOpt[A] = fromFutureA(Future.failed(new Exception(e)))
      override def handleErrorWith[A](fa: FOpt[A])(f: String => FOpt[A]): FOpt[A] =
        EitherT(fa.value.recoverWith { case t => f(t.getMessage).value })
    }
}
