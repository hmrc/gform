/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models

import play.api.libs.json._
import uk.gov.hmrc.gform.Spec

class JsonParseTestPresentationHint extends Spec {

  val startOfJson =
    """
    {
      "id": "gid",
      "label": "glabel"
      """

  "A component without presentationHint" should "parse successfully" in {

    val jsonStr =
      s"""${startOfJson}
         }"""

    var jsr: JsResult[FieldValue] = null
    jsr = implicitly[Reads[FieldValue]].reads(Json.parse(jsonStr));
    println(jsr)
  }

  "A component with a presentationHint that is not a String" should "fail to parse" in {

    for {
      snippet <- List(""", "presentationHint" : 123 }""", """, "presentationHint" : {} }""")
    } {
      val jsResult = implicitly[Reads[FieldValue]].reads(Json.parse(startOfJson + snippet))
      println(jsResult)
      jsResult should be(jsError)
    }
  }

  "A component with a presentationHint that is not a valid string value" should "fail to parse" in {

    for {
      snippet <- List(
        """, "presentationHint" : "collapseGroupUnder" }""",
        """, "presentationHint" : "summarizeGroupAsGrid" }""",
        """, "presentationHint" : "anyString" }"""
      )
    } {
      val jsResult = implicitly[Reads[FieldValue]].reads(Json.parse(startOfJson + snippet))
      println(jsResult)
      jsResult should be(jsError)
    }
  }

  "A component with a valid presentationHint" should "parse correctly" in {

    for {
      snippet <- List(
        """, "presentationHint" : "collapseGroupUnderLabel,summariseGroupAsGrid" }"""
      )
    } {
      val jsResult = implicitly[Reads[FieldValue]].reads(Json.parse(startOfJson + snippet))
      println(jsResult)
      jsResult shouldBe a[JsSuccess[_]]
      jsResult.map(_.presentationHint).get should equal(Some(PresentationHints(List(CollapseGroupUnderLabel, SummariseGroupAsGrid))))
    }
  }

}
