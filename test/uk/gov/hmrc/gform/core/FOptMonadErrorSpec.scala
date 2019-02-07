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

package uk.gov.hmrc.gform.core

import java.util.concurrent.TimeUnit

import uk.gov.hmrc.gform.Spec

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class FOptMonadErrorSpec extends Spec {
  "raiseError" should "work as expected" in {
    expectFutureFailure("hello") {
      fOptMonadError.raiseError("hello")
    }
  }

  private def expectFutureFailure[T](expectedMessage: String)(fopt: FOpt[T]) = {
    var result: Either[String, Unit] = Left("Future not completed yet")

    fopt.value.onSuccess {
      case v => result = Left(s"Expected a Future failure with message '$expectedMessage'. Got success with $v")
    }
    fopt.value.onFailure {
      case t =>
        result =
          if (t.getMessage == expectedMessage) Right(())
          else
            Left(
              s"Expected a Future failure with message '$expectedMessage'. Got a Future failure with message '${t.getMessage}'")
    }

    Await.ready(fopt.value, Duration(2L, TimeUnit.SECONDS))
    result shouldBe Right(())
  }
}
