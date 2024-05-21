/*
 * Copyright 2023 HM Revenue & Customs
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

import scala.language.implicitConversions
import uk.gov.hmrc.gform._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ ExprGen, PrimitiveGen }

class SmartStringSpec extends Spec with ScalaCheckDrivenPropertyChecks {

  implicit def implicitToFormComponentId(str: String): FormComponentId = FormComponentId(str)

  "JSON" should "read a simple String with no embedded interpolations" in {
    forAll(Gen.asciiStr) { s =>
      val c = condition(s)
      val cEscaped = SmartStringTemplateReader.escape(c)
      verifyRead(
        SmartString(LocalisedString(Map(LangADT.En -> cEscaped)), Nil),
        s""""$c""""
      )
    }
  }

  it should "read a simple String with a single embedded interpolation at the beginning" in {
    forAll(Gen.asciiStr) { s1 =>
      val c1 = condition(s1)
      val c1Escaped = SmartStringTemplateReader.escape(c1)

      val fullString = s"$${id0}$c1"

      val expectedSmartString = SmartString(LocalisedString(Map(LangADT.En -> s"{0}$c1Escaped")), List(FormCtx("id0")))

      verifyRead(
        expectedSmartString,
        s""""$fullString""""
      )
    }
  }

  it should "read a simple String with a single embedded interpolation at the end" in {
    forAll(Gen.asciiStr) { s1 =>
      val c1 = condition(s1)
      val c1Escaped = SmartStringTemplateReader.escape(c1)

      val fullString = s"$c1$${id0}"

      val expectedSmartString = SmartString(LocalisedString(Map(LangADT.En -> s"$c1Escaped{0}")), List(FormCtx("id0")))

      verifyRead(
        expectedSmartString,
        s""""$fullString""""
      )
    }
  }

  it should "read a simple String with multiple embedded interpolations" in {
    forAll(Gen.asciiStr, Gen.asciiStr, Gen.asciiStr) { (s1, s2, s3) =>
      val c1 = condition(s1)
      val c1Escaped = SmartStringTemplateReader.escape(c1)
      val c2 = condition(s2)
      val c2Escaped = SmartStringTemplateReader.escape(c2)
      val c3 = condition(s3)
      val c3Escaped = SmartStringTemplateReader.escape(c3)

      val fullString = s"$c1$${id0}$c2$${id2}$c3$${id1}"

      val expectedSmartString = SmartString(
        LocalisedString(Map(LangADT.En -> s"$c1Escaped{0}$c2Escaped{1}$c3Escaped{2}")),
        List(FormCtx("id0"), FormCtx("id2"), FormCtx("id1"))
      )

      verifyRead(
        expectedSmartString,
        s""""$fullString""""
      )
    }
  }

  it should "read a LocalisedString map without interpolations" in {
    forAll(Gen.asciiStr, Gen.asciiStr) { (english, welsh) =>
      val cEnglish = condition(english)
      val cEnglishEscaped = SmartStringTemplateReader.escape(cEnglish)
      val cWelsh = condition(welsh)
      val cWelshEscaped = SmartStringTemplateReader.escape(cWelsh)

      verifyRead(
        SmartString(LocalisedString(Map(LangADT.En -> cEnglishEscaped, LangADT.Cy -> cWelshEscaped)), Nil),
        s"""|{
            |  "en": "$cEnglish",
            |  "cy": "$cWelsh"
            |}""".stripMargin
      )
    }
  }

  it should "read a LocalisedString map with interpolations" in {
    forAll(Gen.asciiStr, Gen.asciiStr, PrimitiveGen.zeroOrMoreGen(ExprGen.exprGen())) { (english, welsh, exprs) =>
      val cEnglish = condition(english) + "{0}"
      val cEnglishEscaped = SmartStringTemplateReader.escape(cEnglish)
      val cWelsh = condition(welsh) + "{1}"
      val cWelshEscaped = SmartStringTemplateReader.escape(cWelsh)
      val interpolations = exprs.map(Expr.format.writes).map(_.toString).mkString(", ")

      verifyRead(
        SmartString(LocalisedString(Map(LangADT.En -> cEnglishEscaped, LangADT.Cy -> cWelshEscaped)), exprs),
        s"""|{
            |  "en": "$cEnglish",
            |  "cy": "$cWelsh",
            |  "interpolations": [ $interpolations ]
            |}""".stripMargin
      )
    }
  }

  it should "read a LocalisedString map with embedded interpolations" in {
    forAll(Gen.asciiStr, Gen.asciiStr) { (english, welsh) =>
      val cEnglish = condition(english)
      val cEnglishEscaped = SmartStringTemplateReader.escape(cEnglish)
      val cWelsh = condition(welsh)
      val cWelshEscaped = SmartStringTemplateReader.escape(cWelsh)

      verifyRead(
        SmartString(
          LocalisedString(Map(LangADT.En -> s"{0}$cEnglishEscaped{1}{2}", LangADT.Cy -> s"{3}$cWelshEscaped{4}{5}")),
          List(FormCtx("id0"), FormCtx("id1"), FormCtx("id2"), FormCtx("id1"), FormCtx("id0"), FormCtx("id2"))
        ),
        s"""|{
            |  "en": "$${id0}$cEnglish$${id1}$${id2}",
            |  "cy": "$${id1}$cWelsh$${id0}$${id2}"
            |}""".stripMargin
      )
    }
  }

  it should "round trip" in {
    forAll(Gen.asciiStr, Gen.asciiStr, PrimitiveGen.zeroOrMoreGen(ExprGen.exprGen())) { (english, welsh, exprs) =>
      val cEnglish = condition(english)
      val cWelsh = condition(welsh)

      val smartString = SmartString(LocalisedString(Map(LangADT.En -> cEnglish, LangADT.Cy -> cWelsh)), exprs)

      verifyRoundTrip(smartString)

    }
  }

  it should "read a SmartStringCond" in {

    val jsonString =
      """|[
         |  {
         |    "en": "Englishstart ${var1} middleEnglish   ${var2} englishend",
         |    "cy": "Welshstart ${var1}   ${var3} welshend",
         |    "includeIf": "${foo contains 0}"
         |  },
         |  {
         |    "en": "${if true then 'foo' else var1} Can you pay ${var3} in full within 30 days?",
         |    "cy": "Can you pay ${var4} in full within 30 days?"
         |  }
         |]""".stripMargin

    val expected: SmartString = SmartString.SmartStringCond(
      List(
        (
          Contains(FormCtx("foo"), Constant("0")),
          SmartStringInternal(
            LocalisedString(
              Map(
                LangADT.En -> "Englishstart {0} middleEnglish   {1} englishend",
                LangADT.Cy -> "Welshstart {2}   {3} welshend"
              )
            ),
            List(FormCtx("var1"), FormCtx("var2"), FormCtx("var1"), FormCtx("var3"))
          )
        )
      ),
      SmartStringInternal(
        LocalisedString(
          Map(
            LangADT.En -> "{0} Can you pay {1} in full within 30 days?",
            LangADT.Cy -> "Can you pay {2} in full within 30 days?"
          )
        ),
        List(IfElse(IsTrue, Constant("foo"), FormCtx("var1")), FormCtx("var3"), FormCtx("var4"))
      )
    )

    verifyRead(
      expected,
      jsonString
    )
  }

  it should "read an Array with single object as SmartStringBase" in {

    val jsonString =
      """|[
         |  {
         |    "en": "${if true then 'foo' else var1} Can you pay ${var3} in full within 30 days?",
         |    "cy": "Can you pay ${var4} in full within 30 days?"
         |  }
         |]""".stripMargin

    val expected: SmartString = SmartString.SmartStringBase(
      SmartStringInternal(
        LocalisedString(
          Map(
            LangADT.En -> "{0} Can you pay {1} in full within 30 days?",
            LangADT.Cy -> "Can you pay {2} in full within 30 days?"
          )
        ),
        List(IfElse(IsTrue, Constant("foo"), FormCtx("var1")), FormCtx("var3"), FormCtx("var4"))
      )
    )

    verifyRead(
      expected,
      jsonString
    )
  }

  it should "ignore _includeIf on last array element" in {
    val jsonString =
      """|[
         |  {
         |    "includeIf": "${1 = 1}",
         |    "en": "EN1",
         |    "cy": "CY1"
         |  },
         |  {
         |    "_includeIf": "${2 = 2}",
         |    "en": "EN2",
         |    "cy": "CY2"
         |  }
         |]""".stripMargin

    val expected: SmartString = SmartString.SmartStringCond(
      List(
        (
          Equals(Constant("1"), Constant("1")),
          SmartStringInternal(LocalisedString(Map(LangADT.En -> "EN1", LangADT.Cy -> "CY1")), List())
        )
      ),
      SmartStringInternal(LocalisedString(Map(LangADT.En -> "EN2", LangADT.Cy -> "CY2")), List())
    )

    verifyRead(
      expected,
      jsonString
    )
  }

  it should "not read a SmartStringConditional with no includeIfs" in {
    val jsonString =
      """|[
         |  {
         |    "en": "Englishstart ${var1} middleEnglish   ${var2} englishend",
         |    "cy": "Welshstart ${var1}   ${var3} welshend"
         |  },
         |  {
         |    "en": "${if true then 'foo' else var1} Can you pay ${var3} in full within 30 days?",
         |    "cy": "Can you pay ${var4} in full within 30 days?"
         |  }
         |]""".stripMargin
    verifyReadFailure[SmartString](
      """IncludeIf is missing. (Only the last object in the array of SmartString objects doesn't have includeIf): {"en":"Englishstart ${var1} middleEnglish   ${var2} englishend","cy":"Welshstart ${var1}   ${var3} welshend"}""",
      jsonString
    )
  }

  it should "not read a SmartStringConditional with all includeIfs" in {
    val jsonString =
      """|[
         |  {
         |    "includeIf": "${foo contains 0}",
         |    "en": "Englishstart ${var1} middleEnglish   ${var2} englishend",
         |    "cy": "Welshstart ${var1}   ${var3} welshend"
         |  },
         |  {
         |    "includeIf": "${foo contains 0}",
         |    "en": "${if true then 'foo' else var1} Can you pay ${var3} in full within 30 days?",
         |    "cy": "Can you pay ${var4} in full within 30 days?"
         |  }
         |]""".stripMargin
    verifyReadFailure[SmartString](
      "The last object in the array SmartString objects should not have includeIf field",
      jsonString
    )
  }

  private def condition(s: String): String =
    s.flatMap { c =>
      if (c >= 32 && c <= 127 && c != '"' && c != '\\' && c != '$') Seq(c)
      else Seq.empty
    }.mkString
}
