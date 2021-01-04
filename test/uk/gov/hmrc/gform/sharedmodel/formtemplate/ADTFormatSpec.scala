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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import org.scalacheck.Gen
import play.api.libs.json.{ Format, JsNumber, JsString, Reads }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.PrimitiveGen
import julienrf.json.derived
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ADTFormatSpec extends Spec with ScalaCheckDrivenPropertyChecks {
  sealed trait EnumerationFoo
  case object EnumerationBar extends EnumerationFoo
  case object EnumerationBaz extends EnumerationFoo

  private val translations: Seq[(String, EnumerationFoo)] = Seq("Bar" -> EnumerationBar, "Baz" -> EnumerationBaz)
  private val allowedNames = translations.map(_._1).toSet

  val fooGen: Gen[EnumerationFoo] = Gen.oneOf(EnumerationBar, EnumerationBaz)
  val fooNameGen: Gen[String] = Gen.oneOf(translations.map(_._1))

  "format" should "write the correct JSON" in {
    val format = ADTFormat.formatEnumeration(translations: _*)

    format.writes(EnumerationBar) shouldBe JsString("Bar")
    format.writes(EnumerationBaz) shouldBe JsString("Baz")
  }

  it should "round trip JSON" in {
    forAll(fooNameGen) { t =>
      verifyRoundTrip(t)
    }
  }

  it should "reject invalid values" in {
    implicit val format: Format[EnumerationFoo] = ADTFormat.formatEnumeration(translations: _*)

    forAll(Gen.alphaNumStr.filterNot(allowedNames)) { s =>
      verifyReadFailure[EnumerationFoo](ADTFormat.invalidReadValue[EnumerationFoo](s, translations: _*), s""""$s"""")
    }
  }

  "formatWithDefault" should "write the correct JSON" in {
    forAll(fooGen) { dflt =>
      val format = ADTFormat.formatEnumerationWithDefault(dflt, translations: _*)
      format.writes(EnumerationBar) shouldBe JsString("Bar")
      format.writes(EnumerationBaz) shouldBe JsString("Baz")
    }
  }

  it should "round trip JSON" in {
    forAll(fooGen, fooGen) { (dflt, t) =>
      implicit val format: Format[EnumerationFoo] = ADTFormat.formatEnumerationWithDefault(dflt, translations: _*)
      verifyRoundTrip(t)
    }
  }

  it should "read an empty string as the default value" in {
    forAll(fooGen) { dflt =>
      implicit val format: Format[EnumerationFoo] = ADTFormat.formatEnumerationWithDefault(dflt, translations: _*)
      verifyRead(dflt, """""""")
    }
  }

  it should "reject invalid values" in {
    forAll(fooGen, PrimitiveGen.nonEmptyAlphaNumStrGen.filterNot(allowedNames)) { (dflt, s) =>
      implicit val format: Format[EnumerationFoo] = ADTFormat.formatEnumerationWithDefault(dflt, translations: _*)
      verifyReadFailure[EnumerationFoo](ADTFormat.invalidReadValue[EnumerationFoo](s, translations: _*), s""""$s"""")
    }
  }

  sealed trait AdtFoo extends Product with Serializable
  case class AdtBar(value: String) extends AdtFoo
  case class AdtBaz(name: String, age: Int) extends AdtFoo

  implicit val adtFooRead: Reads[AdtFoo] =
    ADTFormat.adtRead("type", "bar" -> derived.reads[AdtBar](), "baz" -> derived.reads[AdtBaz]())

  "adtRead" should "correctly read an ADT with a type discriminator" in {

    forAll(Gen.alphaNumStr) { s =>
      verifyRead[AdtFoo](AdtBar(s), s"""{ "type" : "bar", "value" : "$s" }""")
    }

    forAll(Gen.alphaNumStr, Gen.posNum[Int]) { (s, i) =>
      verifyRead[AdtFoo](AdtBaz(s, i), s"""{ "type" : "baz", "name" : "$s", "age" : $i }""")
    }
  }

  it should "reject JSON with a missing type field" in {
    verifyReadFailure[AdtFoo](ADTFormat.missingTypeFieldValue[AdtFoo]("type", Set("bar", "baz")), "{}")
  }

  it should "reject json with an invalid type value" in {
    forAll(Gen.alphaNumStr.filterNot(s => s === "bar" || s === "baz")) { tpe =>
      verifyReadFailure[AdtFoo](
        ADTFormat.invalidTypeValue[AdtFoo]("type", tpe, Set("bar", "baz")),
        s"""{ "type" : "$tpe" }""")
    }
  }

  it should "reject json with an invalid type value type" in {
    verifyReadFailure[AdtFoo](
      ADTFormat.invalidTypeValueType[AdtFoo]("type", JsNumber(1), Set("bar", "baz")),
      s"""{ "type" : 1 }""")
  }
}
