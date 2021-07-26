/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.core.parsers

import cats.Eval
import org.scalatest.prop.TableDrivenPropertyChecks
import parseback.compat.cats._
import org.scalatest.{ FlatSpecLike, Matchers }
import parseback.LineStream

class BasicParsersSpec extends FlatSpecLike with Matchers with TableDrivenPropertyChecks {

  "periodParser" should "parse period constants" in {
    val table = Table(
      ("input", "output"),
      ("P1Y", Some(List("P1Y"))),
      ("P11Y", Some(List("P11Y"))),
      ("P1.1Y", None),
      ("P-1Y", Some(List("P-1Y"))),
      ("P1M", Some(List("P1M"))),
      ("P-1M", Some(List("P-1M"))),
      ("P1D", Some(List("P1D"))),
      ("P-1D", Some(List("P-1D"))),
      ("P1Y1M", Some(List("P1Y1M"))),
      ("P1Y1D", Some(List("P1Y1D"))),
      ("P1M1D", Some(List("P1M1D"))),
      ("P1Y1M1D", Some(List("P1Y1M1D"))),
      ("P1Y-1M-1D", Some(List("P1Y-1M-1D"))),
      ("P+1Y-1M-1D", Some(List("P+1Y-1M-1D"))),
      ("ABC", None)
    )
    forAll(table) { (input, output) =>
      val result = BasicParsers.periodValueParser(LineStream[Eval](input)).value.toOption.map(_.toList)
      result shouldBe output
    }
  }
}
