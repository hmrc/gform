/*
 * Copyright 2022 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, PresentationHint, SummariseGroupAsGrid, TotalValue }

class JsonParseTestPresentationHint extends Spec {

  val startOfJson =
    """
    {
      "id": "gid",
      "label": "glabel",
      "format": "shortText"
      """

  "A component without presentationHint" should "parse successfully" in {

    val jsonStr =
      s"""$startOfJson
         }"""

    var jsr: JsResult[FormComponent] = null
    jsr = implicitly[Reads[FormComponent]].reads(Json.parse(jsonStr));
  }

  "A component with a presentationHint that is not a String" should "fail to parse" in {

    for {
      snippet <- List(""", "presentationHint" : 123 }""", """, "presentationHint" : {} }""")
    } {
      val jsResult = implicitly[Reads[FormComponent]].reads(Json.parse(startOfJson + snippet))
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
      val jsResult = implicitly[Reads[FormComponent]].reads(Json.parse(startOfJson + snippet))
      jsResult should be(jsError)
    }
  }

  "A component with a valid presentationHint" should "parse correctly" in {

    for {
      snippet <- List(""", "presentationHint" : "totalValue, summariseGroupAsGrid" }""")
    } {
      val jsResult = implicitly[Reads[FormComponent]].reads(Json.parse(startOfJson + snippet))
      jsResult shouldBe a[JsSuccess[_]]
      jsResult.map(_.presentationHint).get should equal(Some(List(TotalValue, SummariseGroupAsGrid)))
    }
  }

  "A list of presentation hints" should "transform to and from Json correctly" in {

    val hints = List(TotalValue, SummariseGroupAsGrid)
    Json.parse(Json.toJson(hints).toString()).validate[List[PresentationHint]] shouldBe (JsSuccess(hints))

  }

}
