/*
 * Copyright 2017 HM Revenue & Customs
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

import org.scalactic.source.Position
import org.scalatest.Assertion
import org.scalatest.Assertions.succeed
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.exceptions.TestFailedException
import play.api.libs.json.JsObject
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.models.DbOperationResult
import uk.gov.hmrc.gform.typeclasses.{ FindOne, Insert, Post, Update }

import scala.concurrent.{ ExecutionContext, Future, Promise }
import scala.util.{ Failure, Success }

trait TypeclassCallCheck { def call(): Unit }
trait FindOneCheck extends TypeclassCallCheck
trait UpdateCheck extends TypeclassCallCheck
trait InsertCheck extends TypeclassCallCheck
trait PostCheck extends TypeclassCallCheck

class NotUsedTypeclassError extends Error

trait TypeclassFixtures { self: ScalaFutures =>

  private def noAssertion[A] = (_: A) => succeed
  private def noAssertion2[A, B] = (_: A, _: B) => succeed

  def futureResult[U](future: Future[U])(implicit pos: Position, ec: ExecutionContext): U = {
    handleTestFailedException(future).futureValue match {
      case Left(testFailedException) => throw testFailedException
      case Right(u) => u
    }
  }

  private def handleTestFailedException[U](future: Future[U])(implicit ec: ExecutionContext): Future[Either[TestFailedException, U]] = {
    val p = Promise[Either[TestFailedException, U]]()
    future.onComplete {
      case Failure(t: TestFailedException) => p success Left(t)
      case Failure(t) => p failure (t)
      case Success(v) => p success Right(v)
    }
    p.future
  }

  class FindOneTC[B](callCheck: Option[FindOneCheck], returnValue: Option[B]) {
    def callCheck(callCheck: FindOneCheck) = new FindOneTC(Some(callCheck), returnValue)

    def noChecks = getTC(noAssertion)
    def withChecks(f: JsObject => Assertion) = getTC(f)

    private def getTC(checkFn: JsObject => Assertion): FindOne[B] = new FindOne[B] {
      def apply(selector: JsObject): Future[Option[B]] = {
        callCheck.foreach(_.call())
        checkFn(selector)
        Future.successful(returnValue)
      }
    }
  }

  object FindOneTC {
    def response[A](returnValue: Option[A]) = new FindOneTC(None, returnValue)
    def notUsed[B]: FindOne[B] = new FindOne[B] {
      def apply(selector: JsObject): Future[Option[B]] = {
        Future.failed(new NotUsedTypeclassError)
      }
    }
  }

  class UpdateTC[B](callCheck: Option[UpdateCheck], returnValue: Opt[DbOperationResult]) {
    def callCheck(callCheck: UpdateCheck) = new UpdateTC[B](Some(callCheck), returnValue)

    def noChecks = getTC(noAssertion2)
    def withChecks(f: (JsObject, B) => Assertion) = getTC(f)

    private def getTC(checkFn: (JsObject, B) => Assertion): Update[B] = new Update[B] {
      def apply(selector: JsObject, v: B): Future[Opt[DbOperationResult]] = {
        callCheck.foreach(_.call())
        checkFn(selector, v)
        Future.successful(returnValue)
      }
    }
  }

  object UpdateTC {
    def response[A](returnValue: Opt[DbOperationResult]) = new UpdateTC[A](None, returnValue)
    def notUsed[B]: Update[B] = new Update[B] {
      def apply(selector: JsObject, v: B): Future[Opt[DbOperationResult]] = {
        Future.failed(new NotUsedTypeclassError)
      }
    }
  }

  class InsertTC[B](callCheck: Option[InsertCheck], returnValue: Opt[DbOperationResult]) {
    def callCheck(callCheck: InsertCheck) = new InsertTC[B](Some(callCheck), returnValue)

    def noChecks = getTC(noAssertion2)
    def withChecks(f: (JsObject, B) => Assertion) = getTC(f)

    private def getTC(checkFn: (JsObject, B) => Assertion): Insert[B] = new Insert[B] {
      def apply(selector: JsObject, v: B): Future[Opt[DbOperationResult]] = {
        callCheck.foreach(_.call())
        checkFn(selector, v)
        Future.successful(returnValue)
      }
    }
  }

  object InsertTC {
    def response[A](returnValue: Opt[DbOperationResult]) = new InsertTC[A](None, returnValue)
    def notUsed[B]: Insert[B] = new Insert[B] {
      def apply(selector: JsObject, v: B): Future[Opt[DbOperationResult]] = {
        Future.failed(new NotUsedTypeclassError)
      }
    }
  }

  class PostTC[A, B](callCheck: Option[PostCheck], returnValue: B) {
    def callCheck(callCheck: PostCheck) = new PostTC[A, B](Some(callCheck), returnValue)

    def noChecks = getTC(noAssertion)
    def withChecks(f: A => Assertion) = getTC(f)

    private def getTC(checkFn: A => Assertion): Post[A, B] = new Post[A, B] {
      def apply(v: A): Future[B] = {
        callCheck.foreach(_.call())
        checkFn(v)
        Future.successful(returnValue)
      }
    }
  }

  object PostTC {
    def response[A, B](returnValue: B) = new PostTC[A, B](None, returnValue)
    def notUsed[A, B]: Post[A, B] = new Post[A, B] {
      def apply(a: A): Future[B] = {
        Future.failed(new NotUsedTypeclassError)
      }
    }
  }
}
