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

package uk.gov.hmrc.gform.fileupload

import org.apache.pekko.actor.{ ActorSystem, Scheduler }
import org.scalamock.function.StubFunction0
import scala.concurrent.ExecutionContext
import cats.data.EitherT
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.Spec

import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }

trait RetryingTest extends Retrying {
  def runWith2Retries(
    f: StubFunction0[Future[Int]]
  )(implicit ec: ExecutionContext, s: Scheduler): Future[Int] =
    retry(f(), List(10.milliseconds, 100.milliseconds), "")

  def runEitherWith2Retries(
    f: StubFunction0[EitherT[Future, UnexpectedState, Int]]
  )(implicit ec: ExecutionContext, s: Scheduler): EitherT[Future, UnexpectedState, Int] =
    retryEitherT(f(), List(10.milliseconds, 100.milliseconds), "")
}

class RetryingSpec extends Spec {

  implicit val scheduler: Scheduler = ActorSystem().scheduler

  val retryingTest = new RetryingTest {}

  "Retrying with 3 attempts" should
    "run a test function only once if the first attempt is successful" in {
      val m = stubFunction[Future[Int]]
      m.when().returns(Future.successful(1))
      Await.result(retryingTest.runWith2Retries(m), 2.seconds) should be(1)
      m.verify().once()
    }

  it should "run a test function 3 times and fails if all attempts fail" in {
    val m = stubFunction[Future[Int]]
    inSequence {
      m.when().returns(Future.failed(new Exception("exception1"))).once()
      m.when().returns(Future.failed(new Exception("exception2"))).once()
      m.when().returns(Future.failed(new Exception("exception3"))).once()
    }
    val thrown = intercept[Exception](Await.result(retryingTest.runWith2Retries(m), 2.seconds))
    withClue("the 3-d exception should be thrown") {
      thrown.getMessage should be("exception3")
    }
    m.verify().repeated(3)
  }

  it should "run a test function 3 times and succeeds if the 3-d attempt is successful" in {
    val m = stubFunction[Future[Int]]
    inSequence {
      m.when().returns(Future.failed(new Exception())).twice()
      m.when().returns(Future.successful(1)).once()
    }
    Await.result(retryingTest.runWith2Retries(m), 2.seconds) should be(1)
    m.verify().repeated(3)
  }

  it should "run a test function 3 times and fails if only 4th attempt is successful" in {
    val m = stubFunction[Future[Int]]
    inSequence {
      m.when().returns(Future.failed(new Exception())).repeated(3)
      m.when().returns(Future.successful(1)).once()
    }
    intercept[Exception](Await.result(retryingTest.runWith2Retries(m), 2.seconds))
    Await.result(retryingTest.runWith2Retries(m), 2.seconds) should be(1)
    m.verify().repeated(4)
  }

  it should "retry an EitherT when it returns Left" in {
    val m = stubFunction[EitherT[Future, UnexpectedState, Int]]
    inSequence {
      m.when().returns(EitherT.leftT[Future, Int](UnexpectedState("first"))).once()
      m.when().returns(EitherT.rightT[Future, UnexpectedState](1)).once()
    }
    Await.result(retryingTest.runEitherWith2Retries(m).value, 2.seconds) should be(Right(1))
    m.verify().repeated(2)
  }

  it should "share the retry budget between Left responses and exceptions" in {
    val m = stubFunction[EitherT[Future, UnexpectedState, Int]]
    inSequence {
      m.when().returns(EitherT.leftT[Future, Int](UnexpectedState("first"))).once()
      m.when().returns(EitherT(Future.failed[Either[UnexpectedState, Int]](new Exception("second")))).once()
      m.when().returns(EitherT(Future.failed[Either[UnexpectedState, Int]](new Exception("third")))).once()
    }
    val thrown = intercept[Exception](Await.result(retryingTest.runEitherWith2Retries(m).value, 2.seconds))
    thrown.getMessage should be("third")
    m.verify().repeated(3)
  }

  it should "return the final Left when all EitherT attempts fail" in {
    val m = stubFunction[EitherT[Future, UnexpectedState, Int]]
    inSequence {
      m.when().returns(EitherT.leftT[Future, Int](UnexpectedState("first"))).once()
      m.when().returns(EitherT.leftT[Future, Int](UnexpectedState("second"))).once()
      m.when().returns(EitherT.leftT[Future, Int](UnexpectedState("last"))).once()
    }
    Await.result(retryingTest.runEitherWith2Retries(m).value, 2.seconds) should be(Left(UnexpectedState("last")))
    m.verify().repeated(3)
  }
}
