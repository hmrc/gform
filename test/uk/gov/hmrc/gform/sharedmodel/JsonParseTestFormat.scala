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

import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.libs.json._
import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class JsonParseTestFormat extends Spec with TableDrivenPropertyChecks {

  val startOfJson =
    """
    {
      "id": "gid",
      "label": "glabel"
      """

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
          case Text(constraint, _, DisplayWidth.DEFAULT, _, _, _) =>
            constraint should equal(Number(11, 2, RoundingMode.defaultRoundingMode, None))
          case a @ _ => fail(s"expected a Text, got $a")
        }
      )
    }
  }

  "A component with a valid multiline" should "parse correctly" in {

    val multilineCombinations = Table(
      // format: off
      ("multiline", "expected"),
      (true, TextArea(ShortText.default, Value,dataThreshold = None))
      // format: on
    )

    forAll(multilineCombinations) { (multiline, expected) =>
      val jsResult =
        implicitly[Reads[FormComponent]]
          .reads(Json.parse(startOfJson + s""", "format" : "shortText", "multiline" : $multiline }"""))

      jsResult shouldBe a[JsSuccess[_]]
      jsResult.map(fv => fv.`type` shouldBe expected)

    }
  }

  "A component with invalid multiline" should "default to text" in {
    val table = Table(
      ("multiline", "expected"),
      ("typo", Text(ShortText.default, Value, DisplayWidth.DEFAULT, IsNotUpperCase)),
      ("", Text(ShortText.default, Value, DisplayWidth.DEFAULT, IsNotUpperCase))
    )

    forAll(table) { (multiline, expected) =>
      val jsResult =
        implicitly[Reads[FormComponent]]
          .reads(Json.parse(startOfJson + s""", "format" : "shortText", "multiline" : "$multiline" }"""))
      jsResult should be(jsError)
    }
  }

  "A text component with a valid display width" should "parse correctly" in {

    val displayWidthOptions = Table(
      // format: off
      ("displayWidth", "expected"),
      (DisplayWidth.XS.toString.toLowerCase,  Text(ShortText.default, Value, DisplayWidth.XS)),
      (DisplayWidth.S.toString.toLowerCase,   Text(ShortText.default, Value, DisplayWidth.S)),
      (DisplayWidth.M.toString.toLowerCase,   Text(ShortText.default, Value, DisplayWidth.M)),
      (DisplayWidth.L.toString.toLowerCase,   Text(ShortText.default, Value, DisplayWidth.L)),
      (DisplayWidth.XL.toString.toLowerCase,  Text(ShortText.default, Value, DisplayWidth.XL)),
      (DisplayWidth.XXL.toString.toLowerCase, Text(ShortText.default, Value, DisplayWidth.XXL))
      // format: on
    )

    forAll(displayWidthOptions) { (displayWidth, expected) =>
      val jsResult: JsResult[FormComponent] =
        implicitly[Reads[FormComponent]]
          .reads(Json.parse(startOfJson + s""", "format": "shortText", "displayWidth" : "$displayWidth" }"""))
      jsResult shouldBe a[JsSuccess[_]]
      jsResult.map(fv => fv.`type` shouldBe expected)
    }

  }

  "A text area component with a valid display width" should "parse correctly" in {

    val displayWidthOptions = Table(
      // format: off
      ("displayWidth", "expected"),
      (DisplayWidth.XS.toString.toLowerCase,  TextArea(ShortText.default, Value, DisplayWidth.XS, dataThreshold = None)),
      (DisplayWidth.S.toString.toLowerCase,   TextArea(ShortText.default, Value, DisplayWidth.S, dataThreshold = None)),
      (DisplayWidth.M.toString.toLowerCase,   TextArea(ShortText.default, Value, DisplayWidth.M, dataThreshold = None)),
      (DisplayWidth.L.toString.toLowerCase,   TextArea(ShortText.default, Value, DisplayWidth.L, dataThreshold = None)),
      (DisplayWidth.XL.toString.toLowerCase,  TextArea(ShortText.default, Value, DisplayWidth.XL, dataThreshold = None)),
      (DisplayWidth.XXL.toString.toLowerCase, TextArea(ShortText.default, Value, DisplayWidth.XXL, dataThreshold = None))
      // format: on
    )

    forAll(displayWidthOptions) { (displayWidth, expected) =>
      val jsResult: JsResult[FormComponent] =
        implicitly[Reads[FormComponent]]
          .reads(
            Json.parse(
              startOfJson + s""", "format": "shortText", "multiline" : true """ + s""", "displayWidth" : "$displayWidth" }"""
            )
          )
      jsResult shouldBe a[JsSuccess[_]]
      jsResult.map(fv => fv.`type` shouldBe expected)
    }

  }

  "A text component with an invalid display width format" should "fail to parse" in {

    for {
      snippet <- List(""", "format": "shortText", "displayWidth" : "INVALID DISPLAY WIDTH"}""")
    } {
      val jsResult = implicitly[Reads[FormComponent]].reads(Json.parse(startOfJson + snippet))
      jsResult should beJsSuccess(
        FormComponent(
          FormComponentId("gid"),
          Text(ShortText.default, Value, DisplayWidth.DEFAULT),
          toSmartString("glabel"),
          false,
          None,
          None,
          None,
          None,
          true,
          true,
          true,
          false,
          false,
          None,
          None
        )
      )
    }
  }

  "A text area component with a valid no of rows" should "parse correctly" in {

    val jsResult =
      implicitly[Reads[FormComponent]]
        .reads(Json.parse(startOfJson + s""", "format" : "shortText", "multiline" : true, "rows" : 101 }"""))

    jsResult shouldBe a[JsSuccess[_]]
    jsResult.map(fv => fv.`type` shouldBe TextArea(ShortText.default, Value, rows = 101, dataThreshold = None))
  }

  "A text area component without a no of rows" should "parse correctly to default no of rows" in {

    val jsResult =
      implicitly[Reads[FormComponent]]
        .reads(Json.parse(startOfJson + s""", "format" : "shortText", "multiline" : true }"""))

    jsResult shouldBe a[JsSuccess[_]]
    jsResult.map(fv => fv.`type` shouldBe TextArea(ShortText.default, Value, dataThreshold = None))
  }

  "A text area component with a non positive no of rows" should "fail to parse" in {

    val jsResult =
      implicitly[Reads[FormComponent]]
        .reads(Json.parse(startOfJson + """, "format": "shortText", "multiline" : true, "rows" : -2}"""))

    jsResult should be(jsError)
  }

  "A text area component with a zero no of rows" should "fail to parse" in {

    val jsResult =
      implicitly[Reads[FormComponent]]
        .reads(Json.parse(startOfJson + """, "format": "shortText", "multiline" : true, "rows" : 0}"""))

    jsResult should be(jsError)
  }

  "A text area component with a non-numeric no of rows" should "fail to parse" in {

    val jsResult =
      implicitly[Reads[FormComponent]]
        .reads(
          Json.parse(startOfJson + """, "format": "shortText", "multiline" : true, "rows" : "INVALID NO OF ROWS"}""")
        )

    jsResult should be(jsError)
  }

}
