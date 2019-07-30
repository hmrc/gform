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

package uk.gov.hmrc.gform.sharedmodel

import org.scalatest.{ FlatSpec, Matchers }
import uk.gov.hmrc.gform.typeclasses.Rnd

import scala.util.Random

class AccessCodeSpec extends FlatSpec with Matchers {

  "AccessCode" should "generate access code" in {
    implicit object FixedRandomInt extends Rnd[Int] {
      val r = new Random(12)
      def random(i: Int) = r.nextInt(i)
    }
    AccessCode.random should be(AccessCode("46Q-Z2HW-XIB"))
  }

  it should "generate unique access codes" in {
    val codeA = AccessCode.random
    val codeB = AccessCode.random
    codeA shouldNot be(codeB)
  }
}
