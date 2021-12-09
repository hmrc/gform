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

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{ EitherValues, FlatSpecLike, Matchers }

class BasicParsersSpec extends FlatSpecLike with Matchers with EitherValues with TableDrivenPropertyChecks {

  "anyWordFormatParser" should "parse parse simple text" in {
    BasicParsers.anyWordFormatParser.parseAll("text").right.value shouldBe "text"
  }

  "positiveInteger" should "parse number" in {
    BasicParsers.positiveInteger.parseAll("10").right.value shouldBe 10
  }

  "positiveIntegers" should "parse list of integers" in {
    val ret = BasicParsers.positiveIntegers.parseAll("10 , 20, 30")
    Console.println(s"\n\n $ret \n\n")
    ret.right.value shouldBe (List(10, 20, 30))
  }

  "anyInteger" should "parse any number" in {
    val table = Table(
      ("input", "output"),
      ("+10", 10),
      ("+11", 11),
      ("-11", -11),
      ("11", 11)
    )
    forAll(table) { (input, output) =>
      BasicParsers.anyInteger.parseAll(input).right.value shouldBe output
    }
  }

  "periodParser" should "parse period constants" ignore {
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
      val result = BasicParsers.periodValueParser.parseAll(input).right.toOption
      result shouldBe output
    }
  }
}
