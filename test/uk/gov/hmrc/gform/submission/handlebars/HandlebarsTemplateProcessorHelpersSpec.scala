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

package uk.gov.hmrc.gform.submission.handlebars

import java.text.DecimalFormat

import uk.gov.hmrc.gform.Spec
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.PrimitiveGen

class HandlebarsTemplateProcessorHelpersSpec extends Spec {
  "yesNoToEtmpChoice" must "return 1 when 0 is passed in" in {
    val table = Table(("value", "expected"), ("0", "1"), ("1", "0"))

    forAll(table) {
      case (v, expected) =>
        process(s"{{yesNoToEtmpChoice ${quote(v)}}}") shouldBe expected
    }
  }

  "yesNoToEtmpChoice" must "return null when null" in {
    process("{{yesNoToEtmpChoice null}}") shouldBe "null"
  }

  "dateToEtmpDate" must "return null when null is passed in" in {
    process("{{dateToEtmpDate null}}") shouldBe "null"
  }

  it must "convert a date of the form yyyy-mm-dd into yyyymmdd" in {
    forAll(Gen.chooseNum[Long](1900, 2100), Gen.chooseNum[Long](1, 12), Gen.chooseNum[Long](1, 31)) { (y, m, d) =>
      val yearFormat = new DecimalFormat("0000")
      val monthAndDayFormat = new DecimalFormat("00")
      val inputDate = s"${yearFormat.format(y)}-${monthAndDayFormat.format(m)}-${monthAndDayFormat.format(d)}"
      val outputDate = s"${yearFormat.format(y)}${monthAndDayFormat.format(m)}${monthAndDayFormat.format(d)}"

      process(s"{{dateToEtmpDate ${quote(inputDate)}}}") shouldBe outputDate
    }
  }

  "either" must "select the first argument if it is non-null" in {
    forAll(Gen.alphaNumStr, Gen.alphaNumStr) { (v1, v2) =>
      process(s"{{either null ${quote(v1)} ${quote(v2)}}}") shouldBe v1
      process(s"{{either null ${quote(v1)} null}}") shouldBe v1
    }
  }

  it must "select the second argument if the first is null" in {
    forAll(Gen.alphaNumStr) { v2 =>
      process(s"{{either null null ${quote(v2)}}}") shouldBe v2
    }
  }

  it must "return null if all arguments are null" in {
    process("{{either null null null}}") shouldBe "null"
  }

  "either2" must "select the first argument if it is non-null" in {
    forAll(Gen.alphaNumStr, Gen.alphaNumStr) { (v1, v2) =>
      process(s"{{either2 ${quote(v1)} ${quote(v2)}}}") shouldBe v1
      process(s"{{either2 ${quote(v1)} null}}") shouldBe v1
    }
  }

  it must "select the second argument if the first is null" in {
    forAll(Gen.alphaNumStr) { v2 =>
      process(s"{{either2 null ${quote(v2)}}}") shouldBe v2
    }
  }

  it must "return null if all arguments are null" in {
    process("{{either2 null null}}") shouldBe "null"
  }

  it must "escape backslashes in the output" in {
    process(s"{{either2 ${quote("""hello \ world""")} null}}") shouldBe """hello \\ world"""
  }

  it must "escape single quotes in the output" in {
    process(s"{{either2 ${quote("""hello ' world""")} null}}") shouldBe """hello \' world"""
  }

  "getHmrcTaxPeriodKey" must "return the 0th element of the first non-null, pipe separated parameter" in {
    forAll(listOfNullsAndNonNullsGen(periodGen)(_._4)) {
      case ((key, _, _, _), periods) =>
        process(s"{{getHmrcTaxPeriodKey null $periods}}") shouldBe key
    }
  }

  it must "return null if the period is null" in {
    process("{{getHmrcTaxPeriodKey null null}}") shouldBe "null"
  }

  "getHmrcTaxPeriodFrom" must "return the 1st element of the first non-null, pipe separated parameter" in {
    forAll(listOfNullsAndNonNullsGen(periodGen)(_._4)) {
      case ((_, from, _, _), periods) =>
        process(s"{{getHmrcTaxPeriodFrom null $periods}}") shouldBe from
    }
  }

  it must "return null if the period is null" in {
    process(s"{{getHmrcTaxPeriodFrom null null}}") shouldBe "null"
  }

  "getHmrcTaxPeriodTo" must "return the 2nd element of the first non-null, pipe separated parameter" in {
    forAll(listOfNullsAndNonNullsGen(periodGen)(_._4)) {
      case ((_, _, to, _), periods) =>
        process(s"{{getHmrcTaxPeriodTo null $periods}}") shouldBe to
    }
  }

  it must "return null if the period is null" in {
    process(s"{{getHmrcTaxPeriodTo null null}}") shouldBe "null"
  }

  "isSuccessCode" must "return true for all codes that begin with '2'" in {
    forAll(Gen.chooseNum(200, 299)) { code =>
      whenever(code >= 200 && code <= 299) {
        process(s"{{isSuccessCode $code}}") shouldBe "true"
      }
    }
  }

  it must "return false for all codes that do not begin with '2'" in {
    forAll(Gen.chooseNum(100, 599)) { code =>
      whenever(code < 200 || code >= 300) {
        process(s"{{isSuccessCode $code}}") shouldBe "false"
      }
    }
  }

  it must "return false for a null code" in {
    process(s"{{isSuccessCode null}}") shouldBe "false"
  }

  "isNotSuccessCode" must "return false for all codes that begin with '2'" in {
    forAll(Gen.chooseNum(200, 299)) { code =>
      whenever(code >= 200 && code <= 299) {
        process(s"{{isNotSuccessCode $code}}") shouldBe "false"
      }
    }
  }

  it must "return true for all codes that do not begin with '2'" in {
    forAll(Gen.chooseNum(100, 599)) { code =>
      whenever(code < 200 || code >= 300) {
        process(s"{{isNotSuccessCode $code}}") shouldBe "true"
      }
    }
  }

  it must "return false for a null code" in {
    process(s"{{isNotSuccessCode null}}") shouldBe "false"
  }

  private def periodGen: Gen[(String, String, String, String)] =
    for {
      key  <- Gen.posNum[Int].map(_.toString)
      from <- Gen.posNum[Int].map(_.toString)
      to   <- Gen.posNum[Int].map(_.toString)
    } yield (key, from, to, s"$key|$from|$to")

  private def listOfNullsAndNonNullsGen[G](g: Gen[G])(f: G => String): Gen[(G, String)] = {
    def insertNulls(gs: List[String], acc: List[String]): String =
      if (Math.random < 0.5 && acc.length + gs.length < 10)
        insertNulls(gs, "null" :: acc)
      else
        gs match {
          case Nil    => acc.reverse.mkString(" ")
          case h :: t => insertNulls(t, quote(h) :: acc)
        }

    PrimitiveGen.oneOrMoreGen(g).map(gs => (gs.head, insertNulls(gs.toList.map(f), Nil)))
  }

  private def quote(s: String): String = raw""""$s""""

  private def process(s: String) = new HandlebarsTemplateProcessor()(s, HandlebarsTemplateProcessorModel(""))
}
