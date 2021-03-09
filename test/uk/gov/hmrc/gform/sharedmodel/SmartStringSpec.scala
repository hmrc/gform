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

package uk.gov.hmrc.gform.sharedmodel

import org.scalacheck.Gen
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

  private def condition(s: String): String =
    s.flatMap { c =>
      if (c >= 32 && c <= 127 && c != '"' && c != '\\' && c != '$') Seq(c)
      else Seq.empty
    }.mkString
}
