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
import java.util.Base64

import cats.data.NonEmptyList
import cats.syntax.eq._
import com.fasterxml.jackson.databind.JsonNode
import org.scalacheck.Gen
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.{ PdfHtml, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destinations, HandlebarsTemplateProcessorModel, TemplateType }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, FormTemplateGen, PrimitiveGen }
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.submission.destinations.DestinationsProcessorModelAlgebra

import scala.language.implicitConversions
import scala.util.Random

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

  "toEtmpDate" must "return the date in the given date field as an ETMP formatted date" in {
    val t = Table(
      ("day", "month", "year", "expected"),
      ("", "12", "2019", "null"),
      ("1", "", "2019", "null"),
      ("1", "12", "", "null"),
      ("1", "12", "2019", "20191201"),
      ("20", "05", "2018", "20180520"),
      ("2", "8", "2019", "20190802"),
      ("10", "11", "2020", "20201110")
    )
    forAll(t) {
      case (d, m, y, expected) =>
        process(
          "{{toEtmpDate myDate}}",
          s"""{ "myDate": { "day": "$d", "month": "$m", "year": "$y" } }"""
        ) shouldBe expected
    }
  }

  "toDesDate" must "return the date in the given date field as an DES formatted date" in {
    val t = Table(
      ("day", "month", "year", "expected"),
      ("", "12", "2019", "null"),
      ("1", "", "2019", "null"),
      ("1", "12", "", "null"),
      ("1", "12", "2019", "2019-12-01"),
      ("20", "05", "2018", "2018-05-20"),
      ("2", "8", "2019", "2019-08-02"),
      ("10", "11", "2020", "2020-11-10")
    )
    forAll(t) {
      case (d, m, y, expected) =>
        process(
          "{{toDesDate myDate}}",
          s"""{ "myDate": { "day": "$d", "month": "$m", "year": "$y" } }"""
        ) shouldBe expected
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

  "eitherExcludingBlanks" must "select the first argument if it is non-null and not empty after trimming" in {
    forAll(PrimitiveGen.nonEmptyAlphaNumStrGen, PrimitiveGen.nonEmptyAlphaNumStrGen) { (v1, v2) =>
      process(s"{{eitherExcludingBlanks ${quote(v1)} ${quote(v2)}}}") shouldBe v1
      process(s"{{eitherExcludingBlanks ${quote(v1)} null}}") shouldBe v1
    }
  }

  it must "select the second argument if the first is null" in {
    forAll(PrimitiveGen.nonEmptyAlphaNumStrGen) { v2 =>
      process(s"{{eitherExcludingBlanks null ${quote(v2)}}}") shouldBe v2
    }
  }

  it must "select the second argument if the first is empty after trimming" in {
    forAll(PrimitiveGen.nonEmptyAlphaNumStrGen) { v2 =>
      process(s"{{eitherExcludingBlanks ${quote(" ")} ${quote(v2)}}}") shouldBe v2
    }
  }

  it must "return null if all arguments are null" in {
    process(s"{{eitherExcludingBlanks null null}}") shouldBe "null"
  }

  it must "return null if all arguments are empty after trimming" in {
    process(s"{{eitherExcludingBlanks ${quote(" ")} ${quote(" ")}}}") shouldBe "null"
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

  it must "escape double quotes in the output" in {
    process(s"{{either2 ${quote("""hello " world""")} null}}") shouldBe """hello \" world"""
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

  "toDesAddressWithoutPostcode" must "copy the first 4 lines of full address" in {
    process(
      """{{toDesAddressWithoutPostcode addr}}""",
      """|{
         |  "addr" : {
         |    "street1": "1",
         |    "street2": "The Street",
         |    "street3": "The Town",
         |    "street4": "The County",
         |    "postcode": "The Postcode",
         |    "uk": "true"
         |  }
         |}"""
    ) shouldBe
      """|"addressLine1": "1",
         |"addressLine2": "The Street",
         |"addressLine3": "The Town",
         |"addressLine4": "The County"""".stripMargin
  }

  it must "compact missing lines" in {
    process(
      """{{toDesAddressWithoutPostcode addr}}""",
      """|{
         |  "addr" : {
         |    "street1": "1",
         |    "street2": "",
         |    "street3": "The Town",
         |    "street4": "The County",
         |    "postcode": "The Postcode",
         |    "uk": "true"
         |  }
         |}"""
    ) shouldBe
      """|"addressLine1": "1",
         |"addressLine2": "The Town",
         |"addressLine3": "The County"""".stripMargin
  }

  it must "compact blank (after trimming) lines" in {
    process(
      """{{toDesAddressWithoutPostcode addr}}""",
      """|{
         |  "addr" : {
         |    "street1": "1",
         |    "street2": "",
         |    "street3": " ",
         |    "street4": "The County",
         |    "postcode": "The Postcode",
         |    "uk": "true"
         |  }
         |}"""
    ) shouldBe
      """|"addressLine1": "1",
         |"addressLine2": "The County"""".stripMargin
  }

  it must "emit at least two lines" in {
    process(
      """{{toDesAddressWithoutPostcode addr}}""",
      """|{
         |  "addr" : {
         |    "street1": "1",
         |    "street2": "",
         |    "street3": " ",
         |    "street4": "",
         |    "postcode": "The Postcode",
         |    "uk": "true"
         |  }
         |}"""
    ) shouldBe
      """|"addressLine1": "1",
         |"addressLine2": " """".stripMargin
  }

  "toDesAddressWithoutPostcodeFromArray" must "copy the first 4 lines of full address" in {
    process(
      """{{toDesAddressWithoutPostcodeFromArray addr 1}}""",
      """|{
         |  "addr": [
         |    {"street1": "0", "street2": "The Street", "street3": "The Town", "street4": "Sussex", "postcode": "PC0"},
         |    {"street1": "1", "street2": "The Road", "street3": "The Village", "street4": "Surrey", "postcode": "PC1"},
         |    {"street1": "2", "street2": "The Avenue", "street3": "The City", "street4": "Dorset", "postcode": "PC2"}
         |  ]
         |}"""
    ) shouldBe
      """|"addressLine1": "1",
         |"addressLine2": "The Road",
         |"addressLine3": "The Village",
         |"addressLine4": "Surrey"""".stripMargin
  }

  "match" must "find the appropriate value for a single key value" in {
    process(
      """{{match "('0') => 'A'; ('1') => 'B'; ('3') => 'C'" "1"}}"""
    ) shouldBe "B"
  }

  it must "be able to handle null match values" in {
    process(
      """{{match "('0') => 'A'; (null) => 'B'; ('3') => 'C'" null}}"""
    ) shouldBe "B"
  }

  it must "find the appropriate value for a composite key" in {
    process(
      """{{match "('0' '0') => 'A'; ('1' '0') => 'B'; ('1' '1') => 'C'; ('1' '2') => 'D'" "1" "2"}}"""
    ) shouldBe "D"
  }

  it must "find the appropriate value for a composite with a wildcard" in {
    forAll(Gen.alphaNumStr) { v =>
      process(s"""{{match "('0' *) => 'A'; ('1' '0') => 'B'; ('1' '1') => 'C'; ('1' '2') => 'D'" "0" "$v"}}""") shouldBe "A"
    }
  }

  it must "find the appropriate value for a composite with a null" in {
    process(
      """{{match "('0' '0') => 'A'; ('1' '0') => 'B'; ('1' '1') => 'C'; ('1' null) => 'D'" "1" null}}"""
    ) shouldBe "D"
  }

  it must "compose" in {
    process(
      """{{match "('0' '0') => 'A'; ('1' null) => 'B'; ('1' '1') => 'C'; ('1' '2') => 'D'" (either null "1") (either null null)}}"""
    ) shouldBe "B"
  }

  it must "throw an exception with a good message if the match fails" in {
    def rootCause(t: Throwable): Throwable =
      if (t.getCause == null) t
      else rootCause(t.getCause)

    val cases = "('0' '1') => 'A'"
    rootCause(the[Exception] thrownBy {
      process(s"""{{match "$cases" "1" "2"}}""")
    }) should have message s"""Attempt to match ('1' '2') failed in "$cases""""
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

  "toEtmpParamSequence" must "should work with other numeric functions" in {
    val table = Table(
      ("in", "out"),
      (0, "06"),
      (1, "07"),
      (8, "14"),
      (9, "15"),
      (10, "16"),
      (93, "99")
    )

    forAll(table) {
      case (in, out) =>
        process(s"{{toEtmpParamSequence (plus 5 $in)}}") shouldBe out
    }
  }

  "toEtmpTelephoneNumber" must "return the value untouched if it doesn't start with a '+'" in {
    process("{{toEtmpTelephoneNumber '12345'}}") shouldBe "12345"
  }

  it must "replace initial '+'s with '00'" in {
    process("{{toEtmpTelephoneNumber '+12345'}}") shouldBe "0012345"
  }

  it must "return null if the given inout is null" in {
    process("{{toEtmpTelephoneNumber null}}") shouldBe "null"
  }

  "greaterThan" must "return true if the first parameter is greater than the second, false otherwise" in {
    val table = Table(
      ("first", "second", "result"),
      ("1,000", "0", "true"),
      ("1", "1", "false"),
      ("1", "2", "false")
    )
    forAll(table) {
      case (first, second, result) =>
        process(s"""{{greaterThan "$first" "$second"}}""") shouldBe result
    }
  }

  "lessThan" must "return true if the first parameter is less than the second, false otherwise" in {
    val table = Table(
      ("first", "second", "result"),
      ("1", "0", "false"),
      ("1", "1", "false"),
      ("1", "2,000", "true")
    )
    forAll(table) {
      case (first, second, result) =>
        process(s"""{{lessThan "$first" "$second"}}""") shouldBe result
    }
  }

  "equal" must "return true if the first parameter is equal to the second, false otherwise" in {
    val table = Table(
      ("first", "second", "result"),
      ("1,000", "0", "false"),
      ("1", "1", "true"),
      ("1", "2", "false")
    )
    forAll(table) {
      case (first, second, result) =>
        process(s"""{{equal "$first" "$second"}}""") shouldBe result
    }
  }

  "exists" must "return false if there are no matches" in {
    process("""{{exists foo "1"}}""", """{ "foo": ["2", "3", "4"] }""") shouldBe "false"
  }

  it must "return true if there are matches" in {
    process("""{{exists foo "3"}}""", """{ "foo": ["2", "3", "4"] }""") shouldBe "true"
  }

  "plus" must "add a var-args of numbers together" in {
    val table = Table(
      // format: off
      ("input",   "expected"),
      ("1 2 3",   "6"),
      ("1 1.0",   "2.0"),
      ("1.0 1.0", "2.0"),
      ("1 1.01",  "2.01")
      // format: on
    )

    forAll(table) {
      case (numbers, expected) =>
        process(s"{{plus $numbers}}") shouldBe expected
    }

  }

  "isSigned" must "return true if the formStatus is Signed, false otherwise" in {
    FormStatus.all.foreach { status =>
      process("""{{isSigned}}""", DestinationsProcessorModelAlgebra.createFormStatus(status)) shouldBe (status == Signed).toString
    }
  }

  "isAccepted" must "return true if the formStatus is Accepted, false otherwise" in {
    FormStatus.all.foreach { status =>
      process("""{{isAccepted}}""", DestinationsProcessorModelAlgebra.createFormStatus(status)) shouldBe (status == Accepted).toString
    }
  }

  "isAccepting" must "return true if the formStatus is Accepting, false otherwise" in {
    FormStatus.all.foreach { status =>
      process("""{{isAccepting}}""", DestinationsProcessorModelAlgebra.createFormStatus(status)) shouldBe (status == Accepting).toString
    }
  }

  "isReturning" must "return true if the formStatus is Returning, false otherwise" in {
    FormStatus.all.foreach { status =>
      process("""{{isReturning}}""", DestinationsProcessorModelAlgebra.createFormStatus(status)) shouldBe (status == Returning).toString
    }
  }

  "base64Encode" must "return null if given null" in {
    process("{{base64Encode null}}") shouldBe "null"
  }

  it must "handle arbitrary strings" in {
    forAll(Gen.alphaNumStr) { s =>
      process(s"""{{base64Encode "$s"}}""") shouldBe
        Base64.getEncoder.encodeToString(s.getBytes("UTF-8"))
    }
  }

  "yesNoNull" must "return null when null is given" in {
    process("""{{yesNoNull null}}""") shouldBe "null"
  }

  it must "return 'Yes' when '0' is given" in {
    process("""{{yesNoNull "0"}}""") shouldBe "Yes"
  }

  it must "return 'No' when '1' is given" in {
    process("""{{yesNoNull "1"}}""") shouldBe "No"
  }

  it must "return 'null' when anything else is given" in {
    process("""{{yesNoNull "foo"}}""") shouldBe "null"
  }

  "booleanToYesNo" must "return null when null is given" in {
    process("""{{booleanToYesNo null}}""") shouldBe "null"
  }

  it must "return Yes when true or \"true\" is given" in {
    process("""{{booleanToYesNo true}}""") shouldBe "Yes"
    process("""{{booleanToYesNo "true"}}""") shouldBe "Yes"
  }

  it must "return No when false or \"false\" is given" in {
    process("""{{booleanToYesNo false}}""") shouldBe "No"
    process("""{{booleanToYesNo "false"}}""") shouldBe "No"
  }

  "capitaliseFirst" must "return Null when null is given" in {
    process("""{{capitaliseFirst null}}""") shouldBe "Null"
  }

  it must "capitalise the first letter of any string" in {
    forAll(Gen.alphaNumStr) { s =>
      process(s"""{{capitaliseFirst "$s"}}""") shouldBe s.capitalize
    }
  }

  "indexedLookup" must "return null when null is given" in {
    process("""{{indexedLookup null}}""") shouldBe "null"
  }

  it must "return the value at the index if the index exists" in {
    process("""{{indexedLookup "0" "Foo" "Bar" "Baz"}}""") shouldBe "Foo"
    process("""{{indexedLookup "1" "Foo" "Bar" "Baz"}}""") shouldBe "Bar"
    process("""{{indexedLookup "2" "Foo" "Bar" "Baz"}}""") shouldBe "Baz"
  }

  "normalisePostcode" must "return null when null is given" in {
    process("""{{normalisePostcode null}}""") shouldBe "null"
  }

  it must "return the given value if it has 3 or fewer characters after removing whitespace" in {
    forAll(Gen.alphaNumStr.map(s => if (s.length > 3) s.substring(0, 3) else s), Gen.chooseNum(0, 5)) { (s, ws) =>
      whenever(s.length <= 3) {
        val withSpaces = if (s.isEmpty) s else insertRandomSpaces(s, ws)
        process(s"""{{normalisePostcode "$withSpaces"}}""") shouldBe withSpaces
      }
    }
  }

  it must "return the normalised postcode if it has 4 or more characters after removing whitespace" in {
    forAll(
      PrimitiveGen.nonEmptyAlphaNumStrGen,
      PrimitiveGen.nonEmptyAlphaNumStrGen.filter(_.length >= 3).map(_.substring(0, 3)),
      Gen.chooseNum(0, 5)) { (firstBlock, endTriple, ws) =>
      whenever(!firstBlock.isEmpty && endTriple.length == 3) {
        val withSpaces = insertRandomSpaces(firstBlock + endTriple, ws)
        process(s"""{{normalisePostcode "$withSpaces"}}""") shouldBe s"$firstBlock $endTriple"
      }
    }
  }

  "keyedLookup" must "return the value found at a key" in {
    process(
      """{{keyedLookup foo.bar baz}}""",
      """|{
         |  "foo" : {
         |    "bar" : {
         |      "key1" : "value1",
         |      "key2" : "value2"
         |    }
         |  },
         |  "baz" : "key2"
         |}""".stripMargin
    ) shouldBe "value2"
  }

  it must "return null if the key cannot be found" in {
    process(
      """{{keyedLookup field "key2"}}""",
      """|{
         |  "field" : {
         |    "key" : "12345"
         |  }
         |}""".stripMargin) shouldBe "null"
  }

  "importBySubmissionReference" must "import" in {
    val rootModel: HandlebarsTemplateProcessorModel = HandlebarsTemplateProcessorModel("""{ "foo" : "parent" }""")
    val childModel: HandlebarsTemplateProcessorModel = HandlebarsTemplateProcessorModel("""{ "foo" : "child" }""")

    val childSubmissionReference = SubmissionRef("mySubmissionReference")

    forAll(
      DestinationGen.handlebarsHttpApiGen.map(_.copy(payload = Some("The wrong one."))),
      DestinationGen.handlebarsHttpApiGen.map(_.copy(payload = Some("I am the {{foo}}."))),
      FormTemplateGen.formTemplateGen
    ) { (wrongDestination, destination, formTemplate) =>
      whenever(wrongDestination.id =!= destination.id) {
        val formTemplateWithDestination =
          formTemplate.copy(destinations = Destinations.DestinationList(NonEmptyList.of(wrongDestination, destination)))

        val tree: HandlebarsModelTree =
          HandlebarsModelTree(
            FormId("parentId"),
            SubmissionRef("parent"),
            null,
            PdfHtml(""),
            StructuredFormValue.ObjectStructure(Nil),
            rootModel,
            HandlebarsModelTree(
              FormId("childId"),
              childSubmissionReference,
              formTemplateWithDestination,
              PdfHtml(""),
              StructuredFormValue.ObjectStructure(Nil),
              childModel)
          )

        process(
          s"""I am the {{foo}}. {{importBySubmissionReference "${childSubmissionReference.value}" "${destination.id.id}"}}""",
          FocussedHandlebarsModelTree(tree)) shouldBe "I am the parent. I am the child."
      }
    }
  }

  private def insertRandomSpaces(s: String, numberOfSpaces: Int): String =
    (0 until numberOfSpaces).foldLeft(s) { (acc, _) =>
      val p = Random.nextInt(acc.length)
      acc.substring(0, p) + " " + acc.substring(p)
    }

  private def quote(s: String): String = raw"""'$s'"""

  private def process(functionCall: String, stringModel: String): String = {
    val model = HandlebarsTemplateProcessorModel(stringModel.stripMargin)
    process(
      functionCall,
      FocussedHandlebarsModelTree(
        HandlebarsModelTree(
          FormId("someFormId"),
          SubmissionRef(""),
          null,
          PdfHtml(""),
          StructuredFormValue.ObjectStructure(Nil),
          model))
    )
  }

  private def process(functionCall: String): String =
    process(functionCall, HandlebarsTemplateProcessorModel.empty)

  private def process(functionCall: String, model: HandlebarsTemplateProcessorModel): String =
    process(
      functionCall,
      FocussedHandlebarsModelTree(
        HandlebarsModelTree(
          FormId("someFormId"),
          SubmissionRef(""),
          null,
          PdfHtml(""),
          StructuredFormValue.ObjectStructure(Nil),
          model))
    )

  private def process(functionCall: String, tree: FocussedHandlebarsModelTree): String =
    RealHandlebarsTemplateProcessor(functionCall, HandlebarsTemplateProcessorModel.empty, tree, TemplateType.Plain)
}
