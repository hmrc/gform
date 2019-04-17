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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AnyText, FormComponent, ShortText, Text }

class JsonParseTestValue extends Spec {

  val startOfJson =
    """
    {
      "id": "gid",
      "label": "glabel"
      """

  "A component without value" should "parse successfully" in {

    val jsonStr =
      s"""$startOfJson
         }"""

    implicitly[Reads[FormComponent]].reads(Json.parse(jsonStr)) shouldBe a[JsSuccess[_]]
  }

  "A component with a value that is not a String" should "fail to parse" in {

    for {
      snippet <- List(""", "value" : 123 }""", """, "value" : {} }""")
    } {
      val jsResult = implicitly[Reads[FormComponent]].reads(Json.parse(startOfJson + snippet))
      jsResult should be(jsError)
    }
  }

  "A component with a value that is not a valid string value" should "fail to parse" in {

    for {
      snippet <- List(
                  """, "value" : "65841-351" }""",
                  """, "value" : "${name" }""",
                  """, "value" : "2015-1-12" }""",
                  """, "value" : "201568-01-12" }""")
    } {
      val jsResult = implicitly[Reads[FormComponent]].reads(Json.parse(startOfJson + snippet))
      jsResult should be(jsError)
    }
  }

  "A component with a valid value" should "parse correctly" in {

    for {
      snippet <- List(""", "value" : "'anything'" }""")
    } {
      val jsResult = implicitly[Reads[FormComponent]].reads(Json.parse(startOfJson + snippet))
      jsResult shouldBe a[JsSuccess[_]]
      jsResult.map(fv =>
        fv.`type` match {
          case Text(constraint, _, _, _) => constraint should equal(ShortText)
          case a @ _                     => fail(s"expected a Text, got $a")
      })
    }
  }

}
