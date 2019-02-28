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

import com.fasterxml.jackson.databind.JsonNode
import uk.gov.hmrc.gform.Spec
import org.scalacheck.Gen

import scala.language.implicitConversions

class HandlebarsTemplateProcessorHelpersSpec extends Spec {
  "yesNoToEtmpChoice" must "return 1 when 0 is passed in" in {
    val table = Table(("value", "expected"), ("0", "1"), ("1", "0"), ("0,", "1"), ("1,", "0"))

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
      process(s"{{either ${quote(v1)} ${quote(v2)}}}") shouldBe v1
      process(s"{{either ${quote(v1)} null}}") shouldBe v1
    }
  }

  it must "select the second argument if the first is null" in {
    forAll(Gen.alphaNumStr) { v2 =>
      process(s"{{either null ${quote(v2)}}}") shouldBe v2
    }
  }

  it must "return null if all arguments are null" in {
    process("{{either null null}}") shouldBe "null"
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

  "hmrcTaxPeriodKey" must "return the 0th element of the pipe separated parameter" in {
    forAll(periodGen) {
      case (aKey, _, _, period) =>
        process(s"{{hmrcTaxPeriodKey '$period'}}") shouldBe aKey
    }
  }

  it must "return null if the period is null" in {
    process("{{hmrcTaxPeriodKey null}}") shouldBe "null"
  }

  "hmrcTaxPeriodFrom" must "return the 1st element of the pipe separated parameter" in {
    forAll(periodGen) {
      case (_, from, _, period) =>
        process(s"{{hmrcTaxPeriodFrom '$period'}}") shouldBe from
    }
  }

  it must "return null if the period is null" in {
    process(s"{{hmrcTaxPeriodFrom null}}") shouldBe "null"
  }

  "hmrcTaxPeriodTo" must "return the 2nd element of the pipe separated parameter" in {
    forAll(periodGen) {
      case (_, _, to, period) =>
        process(s"{{hmrcTaxPeriodTo '$period'}}") shouldBe to
    }
  }

  it must "return null if the period is null" in {
    process(s"{{hmrcTaxPeriodTo null}}") shouldBe "null"
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

  it must "return true for a null code" in {
    process("{{isNotSuccessCode null}}") shouldBe "true"
  }

  "removeEmptyAndGet" must "return the default value if there are no options" in {
    process("""{{removeEmptyAndGet "hello" 0}}""") shouldBe "hello"
  }

  it must "return the appropriate value given the index and position of nulls" in {
    val t = Table(
      ("default", "index", "params", "expected"),
      (""""a"""", 0, "", "a"),
      (""""a"""", 0, """"p0" "p1"""", "p0"),
      (""""a"""", 1, """"p0" "p1"""", "p1"),
      (""""b"""", 2, """"p0" "p1"""", "b"),
      (""""b"""", 0, """"" "p1"""", "p1"),
      (""""c"""", 1, """"" "p1"""", "c"),
      (""""c"""", 0, """"" "p1" "" "p3"""", "p1"),
      (""""c"""", 1, """"" "p1" "" "p3"""", "p3"),
      (""""d"""", 2, """"" "p1" "" "p3"""", "d")
    )

    forAll(t) {
      case (dflt, index, params, expected) =>
        process(s"{{removeEmptyAndGet $dflt $index $params}}") shouldBe expected
    }
  }

  "elementAt" must "extract the element at a given index from an array" in {
    process(
      """{{elementAt myArray 1}}""",
      """|{
         |  "myArray" : ["A", "B", "C"]
         |}
      """.stripMargin
    ) shouldBe "B"
  }

  implicit def stringToJacksonTextNode(s: String): JsonNode = {
    import com.fasterxml.jackson.databind.node.JsonNodeFactory.{ instance => jsonNodeFactory }
    jsonNodeFactory.textNode(s)
  }

  "toDesAddressWithoutPostcodeFromArray" must "copy the first 4 lines of full address" in {
    process(
      """{{toDesAddressWithoutPostcodeFromArray "addr" 1}}""",
      """|{
         |  "addr-street1": ["0", "1", "2"],
         |  "addr-street2": ["The Street", "The Road", "The Avenue"],
         |  "addr-street3": ["The Town", "The Village", "The City"],
         |  "addr-street4": ["Sussex", "Surrey", "Dorset"],
         |  "addr-postcode": ["PC0", "PC1", "PC2"],
         |  "addr-uk": "true"
         |}"""
    ) shouldBe
      """|"addressLine1": "1",
         |"addressLine2": "The Road",
         |"addressLine3": "The Village",
         |"addressLine4": "Surrey"""".stripMargin
  }

  "toDesAddressWithoutPostcode" must "copy the first 4 lines of full address" in {
    process(
      """{{toDesAddressWithoutPostcode "addr"}}""",
      """|{
         |  "addr-street1": "1",
         |  "addr-street2": "The Street",
         |  "addr-street3": "The Town",
         |  "addr-street4": "The County",
         |  "addr-postcode": "The Postcode",
         |  "addr-uk": "true"
         |}"""
    ) shouldBe
      """|"addressLine1": "1",
         |"addressLine2": "The Street",
         |"addressLine3": "The Town",
         |"addressLine4": "The County"""".stripMargin
  }

  "it" must "compact missing lines" in {
    process(
      """{{toDesAddressWithoutPostcode "addr"}}""",
      Map[String, JsonNode](
        "addr-street1"  -> "1",
        "addr-street3"  -> "The Town",
        "addr-street4"  -> "The County",
        "addr-postcode" -> "The Postcode",
        "addr-uk"       -> "true"
      )
    ) shouldBe
      """|"addressLine1": "1",
         |"addressLine2": "The Town",
         |"addressLine3": "The County"""".stripMargin
  }

  "it" must "compact blank (after trimming) lines" in {
    process(
      """{{toDesAddressWithoutPostcode "addr"}}""",
      Map[String, JsonNode](
        "addr-street1"  -> "1",
        "addr-street2"  -> "",
        "addr-street3"  -> " ",
        "addr-street4"  -> "The County",
        "addr-postcode" -> "The Postcode",
        "addr-uk"       -> "true"
      )
    ) shouldBe
      """|"addressLine1": "1",
         |"addressLine2": "The County"""".stripMargin
  }

  "it" must "emit at least two lines" in {
    process(
      """{{toDesAddressWithoutPostcode "addr"}}""",
      Map[String, JsonNode](
        "addr-street1"  -> "1",
        "addr-postcode" -> "The Postcode",
        "addr-uk"       -> "true"
      )
    ) shouldBe
      """|"addressLine1": "1",
         |"addressLine2": " """".stripMargin
  }

  "lookup" must "find the appropriate value for a single key value" in {
    process(
      """{{lookup "('0') => 'A'; ('1') => 'B'; ('3') => 'C'" "1"}}"""
    ) shouldBe "B"
  }

  it must "be able to handle null lookup values" in {
    process(
      """{{lookup "('0') => 'A'; (null) => 'B'; ('3') => 'C'" null}}"""
    ) shouldBe "B"
  }

  it must "find the appropriate value for a composite key" in {
    process(
      """{{lookup "('0' '0') => 'A'; ('1' '0') => 'B'; ('1' '1') => 'C'; ('1' '2') => 'D'" "1" "2"}}"""
    ) shouldBe "D"
  }

  it must "find the appropriate value for a composite with a wildcard" in {
    forAll(Gen.alphaNumStr) { v =>
      process(
        s"""{{lookup "('0' *) => 'A'; ('1' '0') => 'B'; ('1' '1') => 'C'; ('1' '2') => 'D'" "0" "$v"}}"""
      ) shouldBe "A"
    }
  }

  it must "find the appropriate value for a composite with a null" in {
    process(
      """{{lookup "('0' '0') => 'A'; ('1' '0') => 'B'; ('1' '1') => 'C'; ('1' null) => 'D'" "1" null}}"""
    ) shouldBe "D"
  }

  it must "compose" in {
    process(
      """{{lookup "('0' '0') => 'A'; ('1' null) => 'B'; ('1' '1') => 'C'; ('1' '2') => 'D'" (either null "1") (either null null)}}"""
    ) shouldBe "B"
  }

  it must "throw an exception with a good message if the lookup fails" in {
    def rootCause(t: Throwable): Throwable =
      if (t.getCause == null) t
      else rootCause(t.getCause)

    val cases = "('0' '1') => 'A'"
    rootCause(the[Exception] thrownBy {
      process(s"""{{lookup "$cases" "1" "2"}}""")
    }) should have message s"""Attempt to lookup ('1' '2') failed in "$cases""""
  }

  "stripCommas" must "return null if the argument is null" in {
    process("{{stripCommas null}}") shouldBe "null"
  }

  it must "remove all commas from strings" in {
    process("{{stripCommas 'abc,def,ghi'}}") shouldBe "abcdefghi"
  }

  it must "compose" in {
    process("{{stripCommas (either null \"12,345\")}}") shouldBe "12345"
  }

  "not" must "return null if the argument is null" in {
    process("{{not null}}") shouldBe "null"
  }

  it must "invert boolean values" in {
    val table = Table(("value", "expected"), ("false", "true"), ("true", "false"))

    forAll(table) {
      case (v, expected) =>
        process(s"{{not ${quote(v)}}}") shouldBe expected
    }
  }

  "or" must "return false if all arguments are null" in {
    process("{{or null null}}") shouldBe "false"
  }

  it must "or boolean values, ignoring nulls" in {
    val table = Table(
      ("one", "two", "expected"),
      ("false", "false", "false"),
      ("false", "true", "true"),
      ("true", "true", "true"),
      ("true", "false", "true")
    )

    forAll(table) {
      case (v1, v2, expected) =>
        process(s"{{or ${quote(v1)} ${quote(v2)} null}}") shouldBe expected
        process(s"{{or ${quote(v1)} null ${quote(v2)}}}") shouldBe expected
        process(s"{{or null ${quote(v1)} ${quote(v2)}}}") shouldBe expected
    }
  }

  "and" must "return true if all arguments are null" in {
    process("{{and null null}}") shouldBe "true"
  }

  it must "and boolean values, ignoring nulls" in {
    val table = Table(
      ("one", "two", "expected"),
      ("false", "false", "false"),
      ("false", "true", "false"),
      ("true", "true", "true"),
      ("true", "false", "false")
    )

    forAll(table) {
      case (v1, v2, expected) =>
        process(s"{{and ${quote(v1)} ${quote(v2)} null}}") shouldBe expected
        process(s"{{and ${quote(v1)} null ${quote(v2)}}}") shouldBe expected
        process(s"{{and null ${quote(v1)} ${quote(v2)}}}") shouldBe expected
    }
  }

  "boolean functions" must "compose" in {
    process("{{not (and 'true' (or 'false' 'false' 'true'))}}") shouldBe "false"
    process("{{not (and (or 'false' 'false') 'true' (or 'false' 'true'))}}") shouldBe "true"
    process("{{not (and (not (or 'false' 'false')) 'true' (or 'false' 'true'))}}") shouldBe "false"
  }

  "isNull" must "return 'true' when the argument is null" in {
    process("{{isNull null}}") shouldBe "true"
  }

  it must "return 'false' otherwise" in {
    process("{{isNull \"foo\"}}") shouldBe "false"
  }

  it must "compose" in {
    process("{{isNull (either null null)}}") shouldBe "true"
    process("{{isNull (either null \"123\")}}") shouldBe "false"
  }

  "isNotNull" must "return 'false' when the argument is null" in {
    process("{{isNotNull null}}") shouldBe "false"
  }

  it must "return 'true' otherwise" in {
    process("{{isNotNull \"123\"}}") shouldBe "true"
  }

  it must "compose" in {
    process("{{isNotNull (either null null)}}") shouldBe "false"
    process("{{isNotNull (either null \"123\")}}") shouldBe "true"
  }

  "toEtmpParamSequence" must "add one to the given index and pad-left with 0 if it is less than or equal to 9" in {
    val table = Table(
      ("in", "out"),
      (0, "01"),
      (1, "02"),
      (8, "09"),
      (9, "10"),
      (10, "11"),
      (98, "99")
    )

    forAll(table) {
      case (in, out) =>
        process(s"{{toEtmpParamSequence $in}}") shouldBe out
    }
  }

  private def periodGen: Gen[(String, String, String, String)] =
    for {
      key  <- Gen.posNum[Int].map(_.toString)
      from <- Gen.posNum[Int].map(_.toString)
      to   <- Gen.posNum[Int].map(_.toString)
    } yield (key, from, to, s"$key|$from|$to")

  private def quote(s: String): String = raw""""$s""""

  private def process(functionCall: String, model: String): String =
    process(functionCall, HandlebarsTemplateProcessorModel(model.stripMargin))

  private def process(functionCall: String, formFields: Map[String, JsonNode] = Map.empty): String =
    process(functionCall, HandlebarsTemplateProcessorModel(formFields))

  private def process(functionCall: String, model: HandlebarsTemplateProcessorModel): String =
    new HandlebarsTemplateProcessor()(functionCall, model)
}
