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

package uk.gov.hmrc.gform.sharedmodel

import play.api.libs.json._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class JsonParseTestFormat extends Spec {

  val startOfJson =
    """
    {
      "id": "gid",
      "label": "glabel"
      """

  "A component without format" should "parse successfully" in {

    val jsonStr =
      s"""$startOfJson
         }"""

    implicitly[Reads[FormComponent]].reads(Json.parse(jsonStr)) shouldBe a[JsSuccess[_]]
  }

  "A component with a format that is not a String" should "fail to parse" in {

    for {
      snippet <- List(""", "format" : 123 }""", """, "format" : {} }""")
    } {
      val jsResult = implicitly[Reads[FormComponent]].reads(Json.parse(startOfJson + snippet))

      jsResult should be(jsError)
    }
  }

  "A component with a format that is not a valid string value" should "fail to parse" in {

    for {
      snippet <- List(""", "format" : "befor 2017-04-02,after 2017-02-01" }""", """, "format" : "before 201-04-02" }""")
    } {
      val jsResult = implicitly[Reads[FormComponent]].reads(Json.parse(startOfJson + snippet))
      jsResult should be(jsError)
    }
  }

  "A component with a valid format" should "parse correctly" in {

    for {
      snippet <- List(""", "format" : "number" }""")
    } {
      val jsResult = implicitly[Reads[FormComponent]].reads(Json.parse(startOfJson + snippet))
      jsResult shouldBe a[JsSuccess[_]]
      jsResult.map(fv =>
        fv.`type` match {
          case Text(constraint, _, DisplayWidth.DEFAULT, _) =>
            constraint should equal(Number(11, 2, RoundingMode.defaultRoundingMode, None))
          case a @ _ => fail(s"expected a Text, got $a")
      })
    }
  }

  "A component with a valid multiline" should "parse correctly" in {

    val multilineCombinations = Table(
      // format: off
      ("multiline", "expected"),
      ("yes",  TextArea(BasicText, Value)),
      ("Yes",  TextArea(BasicText, Value)),
      ("true", TextArea(BasicText, Value)),
      ("True", TextArea(BasicText, Value)),
      ("no",   Text(ShortText, Value, DisplayWidth.DEFAULT)),
      ("typo", Text(ShortText, Value, DisplayWidth.DEFAULT)),
      ("",     Text(ShortText, Value, DisplayWidth.DEFAULT))
      // format: on
    )

    forAll(multilineCombinations) { (multiline, expected) ⇒
      val jsResult =
        implicitly[Reads[FormComponent]].reads(Json.parse(startOfJson + s""", "multiline" : "$multiline" }"""))

      jsResult shouldBe a[JsSuccess[_]]
      jsResult.map(fv => fv.`type` shouldBe expected)

    }
  }

  "A text component with a valid display width" should "parse correctly" in {

    val displayWidthOptions = Table(
      // format: off
      ("displayWidth", "expected"),
      (DisplayWidth.XS.toString.toLowerCase,  Text(ShortText, Value, DisplayWidth.XS)),
      (DisplayWidth.S.toString.toLowerCase,   Text(ShortText, Value, DisplayWidth.S)),
      (DisplayWidth.M.toString.toLowerCase,   Text(ShortText, Value, DisplayWidth.M)),
      (DisplayWidth.L.toString.toLowerCase,   Text(ShortText, Value, DisplayWidth.L)),
      (DisplayWidth.XL.toString.toLowerCase,  Text(ShortText, Value, DisplayWidth.XL)),
      (DisplayWidth.XXL.toString.toLowerCase, Text(ShortText, Value, DisplayWidth.XXL))
      // format: on
    )

    forAll(displayWidthOptions) { (displayWidth, expected) =>
      val jsResult: JsResult[FormComponent] =
        implicitly[Reads[FormComponent]].reads(Json.parse(startOfJson + s""", "displayWidth" : "$displayWidth" }"""))
      jsResult shouldBe a[JsSuccess[_]]
      jsResult.map(fv => fv.`type` shouldBe expected)
    }

  }

  "A text area component with a valid display width" should "parse correctly" in {

    val displayWidthOptions = Table(
      // format: off
      ("displayWidth", "expected"),
      (DisplayWidth.XS.toString.toLowerCase,  TextArea(BasicText, Value, DisplayWidth.XS)),
      (DisplayWidth.S.toString.toLowerCase,   TextArea(BasicText, Value, DisplayWidth.S)),
      (DisplayWidth.M.toString.toLowerCase,   TextArea(BasicText, Value, DisplayWidth.M)),
      (DisplayWidth.L.toString.toLowerCase,   TextArea(BasicText, Value, DisplayWidth.L)),
      (DisplayWidth.XL.toString.toLowerCase,  TextArea(BasicText, Value, DisplayWidth.XL)),
      (DisplayWidth.XXL.toString.toLowerCase, TextArea(BasicText, Value, DisplayWidth.XXL))
      // format: on
    )

    forAll(displayWidthOptions) { (displayWidth, expected) =>
      val jsResult: JsResult[FormComponent] =
        implicitly[Reads[FormComponent]]
          .reads(Json.parse(startOfJson + s""", "multiline" : "true" """ + s""", "displayWidth" : "$displayWidth" }"""))
      jsResult shouldBe a[JsSuccess[_]]
      jsResult.map(fv => fv.`type` shouldBe expected)
    }

  }

  "A text component with an invalid display width format" should "fail to parse" in {

    for {
      snippet <- List(""", "displayWidth" : "INVALID DISPLAY WIDTH"}""")
    } {
      val jsResult = implicitly[Reads[FormComponent]].reads(Json.parse(startOfJson + snippet))
      jsResult should beJsSuccess(
        FormComponent(
          FormComponentId("gid"),
          Text(ShortText, Value, DisplayWidth.DEFAULT),
          "glabel",
          None,
          None,
          None,
          true,
          true,
          true,
          false,
          false,
          None,
          None))
    }
  }

}
