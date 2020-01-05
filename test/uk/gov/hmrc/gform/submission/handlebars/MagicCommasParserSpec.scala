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

package uk.gov.hmrc.gform.submission.handlebars

import cats.syntax.eq._
import cats.instances.char._
import org.scalacheck.Gen
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.PrimitiveGen

class MagicCommasParserSpec extends Spec {
  private def stringWithNoSpecialCharsGen: Gen[String] =
    Gen.asciiPrintableStr.map(
      _.replace(",", "")
        .replace("\"", "")
        .replace("[", "")
        .replace("]", ""))

  "apply" should "return any string that is inputted" in {
    forAll(stringWithNoSpecialCharsGen) { input =>
      whenever(isUn(input)) {
        verifySuccess(input, input)
      }
    }
  }

  it should "replace commas followed by a square bracket with just the bracket" in {
    forAll(
      stringWithNoSpecialCharsGen,
      PrimitiveGen.zeroOrMoreGen(Gen.oneOf(" ", "\n", "\r", "\t")).map(_.mkString("")),
      stringWithNoSpecialCharsGen) { (beforeCommas, spaces, afterCommas) =>
      whenever(isUn(beforeCommas, afterCommas)) {
        verifySuccess(s"$beforeCommas,$spaces]$afterCommas", s"$beforeCommas$spaces]$afterCommas")
      }
    }
  }

  it should "replace commas followed by a curly bracket with just the curly bracket" in {
    forAll(
      stringWithNoSpecialCharsGen,
      PrimitiveGen.zeroOrMoreGen(Gen.oneOf(" ", "\n", "\r", "\t")).map(_.mkString("")),
      stringWithNoSpecialCharsGen) { (beforeCommas, spaces, afterCommas) =>
      whenever(isUn(beforeCommas, afterCommas)) {
        verifySuccess(s"$beforeCommas,$spaces}$afterCommas", s"$beforeCommas$spaces}$afterCommas")
      }
    }
  }

  it should "copy quoted strings verbatim" in {
    forAll(stringWithNoSpecialCharsGen, Gen.oneOf(",,", ",,[", ",, ["), stringWithNoSpecialCharsGen) {
      (beforeCommas, specialChars, afterCommas) =>
        val quotedString = s""""$beforeCommas$specialChars$afterCommas""""
        verifySuccess(quotedString, quotedString)
    }
  }

  it should "copy unclosed literal strings as if they were not literal strings" in {
    forAll(stringWithNoSpecialCharsGen) { s =>
      val ss = s""""s"""
      verifySuccess(ss, ss)
    }
  }

  it should "correctly transform a JSON document with magic commas" in {
    val input =
      """{
        |  "field1" : "hello",
        |  "field2" : [
        |  ],
        |  "field3" : [
        |     1,
        |     2,
        |     3,
        |  ],
        |  "field4" : [
        |     1,
        |     2,
        |     3
        |  ],
        |   "field5" : [
        |     { "field6" : "abc"},
        |     { "field7" : "def"},
        |  ],
        |   "field5" : [
        |     { "field8" : [
        |       1,
        |       2,
        |     ]
        |    }
        |  ]
        |}
      """.stripMargin

    val expected =
      """{
        |  "field1" : "hello",
        |  "field2" : [
        |  ],
        |  "field3" : [
        |     1,
        |     2,
        |     3
        |  ],
        |  "field4" : [
        |     1,
        |     2,
        |     3
        |  ],
        |   "field5" : [
        |     { "field6" : "abc"},
        |     { "field7" : "def"}
        |  ],
        |   "field5" : [
        |     { "field8" : [
        |       1,
        |       2
        |     ]
        |    }
        |  ]
        |}
      """.stripMargin

    verifySuccess(input, expected)
  }

  private def verifySuccess(input: String, expected: String) =
    MagicCommasParser(input) shouldBe expected

  private def isUn(s: String*) =
    isAscii(s: _*) && hasNoDoubleCommas(s: _*) && hasNoFinalCommas(s: _*) && hasNoInitialClosingSquareBrackets(s: _*) && hasNoQuotes(
      s: _*)

  private def isAscii(s: String*): Boolean = s.forall(_.forall(c => c >= 32 && c <= 126))
  private def hasNoFinalCommas(s: String*): Boolean = s.forall(s => s.isEmpty || s.last =!= ',')
  private def hasNoInitialClosingSquareBrackets(s: String*): Boolean = s.forall(s => s.isEmpty || s.head =!= ']')
  private def hasNoDoubleCommas(ss: String*): Boolean = ss.forall(s => !s.contains(",,"))
  private def hasNoQuotes(ss: String*): Boolean = ss.forall(s => !s.contains("\""))
}
