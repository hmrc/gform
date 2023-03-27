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

import akka.actor.{ ActorSystem, Scheduler }
import org.scalamock.function.StubFunction0
import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.Spec

import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }

trait RetryingTest extends Retrying {
  def runWith3Retries(
    f: StubFunction0[Future[Int]]
  )(implicit ec: ExecutionContext, s: Scheduler): Future[Int] =
    retry(f(), Seq(10.milliseconds, 100.milliseconds), "")
}

class RetryingSpec extends Spec {

  implicit val scheduler: Scheduler = ActorSystem().scheduler

  val retryingTest = new RetryingTest {}

  "Retrying with 3 attempts" should
    "run a test function only once if the first attempt is successful" in {
      val m = stubFunction[Future[Int]]
      m.when().returns(Future.successful(1))
      Await.result(retryingTest.runWith3Retries(m), 2.seconds) should be(1)
      m.verify().once()
    }

  it should "run a test function 3 times and fails if all attempts fail" in {
    val m = stubFunction[Future[Int]]
    inSequence {
      m.when().returns(Future.failed(new Exception("exception1"))).once()
      m.when().returns(Future.failed(new Exception("exception2"))).once()
      m.when().returns(Future.failed(new Exception("exception3"))).once()
    }
    val thrown = intercept[Exception](Await.result(retryingTest.runWith3Retries(m), 2.seconds))
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
    Await.result(retryingTest.runWith3Retries(m), 2.seconds) should be(1)
    m.verify().repeated(3)
  }

  it should "run a test function 3 times and fails if only 4th attempt is successful" in {
    val m = stubFunction[Future[Int]]
    inSequence {
      m.when().returns(Future.failed(new Exception())).repeated(3)
      m.when().returns(Future.successful(1)).once()
    }
    intercept[Exception](Await.result(retryingTest.runWith3Retries(m), 2.seconds))
    Await.result(retryingTest.runWith3Retries(m), 2.seconds) should be(1)
    m.verify().repeated(4)
  }
}
