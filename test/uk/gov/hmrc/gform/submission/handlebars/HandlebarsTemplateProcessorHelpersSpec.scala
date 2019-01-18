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
import java.time.LocalDateTime

import uk.gov.hmrc.gform.Spec
import org.scalacheck.Gen
import uk.gov.hmrc.gform.time.TimeProvider

class HandlebarsTemplateProcessorHelpersSpec extends Spec {
  val helper = new HandlebarsTemplateProcessorHelpers
  import helper._

  "yesNoToEtmpChoice" must "return 1 when 0 is passed in" in {
    yesNoToEtmpChoice("0") shouldBe "1"
  }

  it must "return 0 when 1 is passed in" in {
    yesNoToEtmpChoice("1") shouldBe "0"
  }

  "dateToEtmpDate" must "return null when null is passed in" in {
    dateToEtmpDate(null) shouldBe null
  }

  it must "convert a date of the form yyyy-mm-dd into yyyymmdd" in {
    forAll(Gen.chooseNum[Long](1900, 2100), Gen.chooseNum[Long](1, 12), Gen.chooseNum[Long](1, 31)) { (y, m, d) =>
      val yearFormat = new DecimalFormat("0000")
      val monthAndDayFormat = new DecimalFormat("00")
      val inputDate = s"${yearFormat.format(y)}-${monthAndDayFormat.format(m)}-${monthAndDayFormat.format(d)}"
      val outputDate = s"${yearFormat.format(y)}${monthAndDayFormat.format(m)}${monthAndDayFormat.format(d)}"

      dateToEtmpDate(inputDate) shouldBe outputDate
    }
  }

  "either" must "select the first argument if it is non-null" in {
    forAll(Gen.alphaNumStr, Gen.alphaNumStr) { (v1, v2) =>
      either(v1, v2) shouldBe v1
      either(v1, null) shouldBe v1
    }
  }

  it must "select the second argument if the first is null" in {
    forAll(Gen.alphaNumStr) { v2 =>
      either(null, v2) shouldBe v2
    }
  }

  it must "return null if both arguments are null" in {
    either(null, null) shouldBe null
  }

  "getPeriodKey" must "return the 0th element of the pipe separated parameter" in {
    forAll(periodGen) {
      case (key, _, _, period) =>
        getPeriodKey(period) shouldBe key
    }
  }

  it must "return null if the period is null" in {
    getPeriodKey(null) shouldBe null
  }

  "getPeriodFrom" must "return the 0th element of the pipe separated parameter" in {
    forAll(periodGen) {
      case (_, from, _, period) =>
        getPeriodFrom(period) shouldBe from
    }
  }

  it must "return null if the period is null" in {
    getPeriodFrom(null) shouldBe null
  }

  "getPeriodTo" must "return the 0th element of the pipe separated parameter" in {
    forAll(periodGen) {
      case (_, _, to, period) =>
        getPeriodTo(period) shouldBe to
    }
  }

  "getCurrentDate" must "return the current date formatted in yyyymmdd format" in {
    val padLeft = new DecimalFormat("00")

    forAll(Gen.chooseNum(1900, 2100), Gen.chooseNum(1, 12), Gen.chooseNum(1, 28)) { (year, month, day) =>
      whenever(year >= 1900 && year <= 2100 && month >= 1 && month <= 12 && day >= 1 && day <= 28) {
        val timeProvider = mock[TimeProvider]
        val helpers = new HandlebarsTemplateProcessorHelpers(timeProvider)

        (timeProvider.localDateTime _)
          .expects()
          .returning(LocalDateTime.of(year, month, day, 0, 0))

        val expected = s"$year${padLeft.format(month)}${padLeft.format(day)}"

        helpers.getCurrentDate shouldBe expected
      }
    }
  }

  it must "return null if the period is null" in {
    getPeriodTo(null) shouldBe null
  }

  private def periodGen: Gen[(String, String, String, String)] =
    for {
      key  <- Gen.posNum[Int].map(_.toString)
      from <- Gen.posNum[Int].map(_.toString)
      to   <- Gen.posNum[Int].map(_.toString)
    } yield (key, from, to, s"$key|$from|$to")
}
