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

package uk.gov.hmrc

import cats.MonadError

import scala.annotation.tailrec

package object gform {
  type Possible[T] = Either[String, T]
  implicit val possibleMonadError: MonadError[Possible, String] = new MonadError[Possible, String] {
    override def flatMap[A, B](fa: Possible[A])(f: A => Possible[B]): Possible[B] = fa.flatMap(f)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Possible[Either[A, B]]): Possible[B] = f(a) match {
      case Left(s)         => Left(s)
      case Right(Left(a2)) => tailRecM(a2)(f)
      case Right(Right(b)) => Right(b)
    }

    override def raiseError[A](e: String): Possible[A] = Left(e)

    override def handleErrorWith[A](fa: Possible[A])(f: String => Possible[A]): Possible[A] = fa match {
      case Left(s)  => f(s)
      case Right(a) => Right(a)
    }

    override def pure[A](a: A): Possible[A] = Right(a)
  }
}
