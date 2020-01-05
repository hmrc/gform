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

package uk.gov.hmrc.gform.core

import uk.gov.hmrc.gform.Spec

class FOptMonadErrorSpec extends Spec {
  "raiseError" should "work as expected" in {
    expectFutureFailure("hello") {
      fOptMonadError.raiseError("hello")
    }
  }

  "flatMap" should "flatMap" in {
    fOptMonadError
      .flatMap(success(1)) { x =>
        success(x * 2)
      }
      .value
      .futureValue shouldBe Right(2)
  }

  "handleErrorWith" should "handle the error" in {
    expectFutureFailure("Hello, world") {
      fOptMonadError
        .handleErrorWith(fOptMonadError.raiseError[Unit]("Hello")) { x =>
          fOptMonadError.raiseError(x + ", world")
        }
    }
  }

  private def expectFutureFailure[T](expectedMessage: String)(fopt: FOpt[T]) =
    try {
      fopt.value.futureValue
      fail("Expected Future to be a failure")
    } catch {
      case e: Exception => e.getCause.getMessage shouldBe expectedMessage
    }
}
