/*
 * Copyright 2018 HM Revenue & Customs
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
          case Text(constraint, _, DisplayWidthAttribute.L) => constraint should equal(Number(11, 2, None))
          case a @ _                                        => fail(s"expected a Text, got $a")
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
      ("no",   Text(ShortText, Value, DisplayWidthAttribute.L)),
      ("typo", Text(ShortText, Value, DisplayWidthAttribute.L)),
      ("",     Text(ShortText, Value, DisplayWidthAttribute.L))
      // format: on
    )

    forAll(multilineCombinations) { (multiline, expected) ⇒
      val jsResult =
        implicitly[Reads[FormComponent]].reads(Json.parse(startOfJson + s""", "multiline" : "$multiline" }"""))

      jsResult shouldBe a[JsSuccess[_]]
      jsResult.map(fv => fv.`type` shouldBe expected)

    }
  }

  "A component with a valid display width" should "parse correctly" in {

    val displayWidthOptions = Table(
      // format: off
      ("displayWidth", "expected"),
      (DisplayWidthAttribute.XS.toString.toLowerCase,  Text(ShortText, Value, DisplayWidthAttribute.XS)),
      (DisplayWidthAttribute.S.toString.toLowerCase,   Text(ShortText, Value, DisplayWidthAttribute.S)),
      (DisplayWidthAttribute.M.toString.toLowerCase,   Text(ShortText, Value, DisplayWidthAttribute.M)),
      (DisplayWidthAttribute.L.toString.toLowerCase,   Text(ShortText, Value, DisplayWidthAttribute.L)),
      (DisplayWidthAttribute.XL.toString.toLowerCase,  Text(ShortText, Value, DisplayWidthAttribute.XL)),
      (DisplayWidthAttribute.XXL.toString.toLowerCase, Text(ShortText, Value, DisplayWidthAttribute.XXL))
      // format: on
    )

    forAll(displayWidthOptions) { (displayWidth, expected) =>
      val jsResult: JsResult[FormComponent] =
        implicitly[Reads[FormComponent]].reads(Json.parse(startOfJson + s""", "displayWidth" : "$displayWidth" }"""))
      jsResult shouldBe a[JsSuccess[_]]
      jsResult.map(fv => fv.`type` shouldBe expected)
    }

  }

}
