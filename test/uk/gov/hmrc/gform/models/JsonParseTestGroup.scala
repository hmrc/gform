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

import cats.data.NonEmptyList
import play.api.libs.json._
import uk.gov.hmrc.gform.Spec

class JsonParseTestGroup extends Spec {

  "A raw group" should "parse" in {

    val jsonStr =
      """
      {
        "type": "group",
        "id": "gid",
        "label": "glabel",
        "format" : "horizontal",
        "repeatsMin":1,
        "repeatsMax":5,
        "repeatLabel":"repeatLabel",
        "repeatAddAnotherText":"repeatAddAnotherText",
        "fields": [
          {
            "type": "choice",
            "id": "cid",
            "label": "clabel",
            "choices": [
              "A",
              "B"
            ]
          }
        ],
        "presentationHint" : "collapseGroupUnderLabel"

      }
    """

    val jsResult = implicitly[Reads[FieldValue]].reads(Json.parse(jsonStr))

    jsResult should beJsSuccess(FieldValue(
      FieldId("gid"),
      Group(
        List(
          FieldValue(
            FieldId("cid"),
            Choice(Radio, NonEmptyList.of("A", "B"), Vertical, List(), None), "clabel", None, None, true, true, true, None
          )
        ),
        Horizontal,
        Some(5), Some(1), Some("repeatLabel"), Some("repeatAddAnotherText")
      ),
      "glabel", None, None, true, true, true,
      None,
      Some(List(CollapseGroupUnderLabel))
    ))

  }

  "A raw group" should "fail to parse if repeatsMin/Max has errors" in {

    val jsonStr =
      """
      {
        "type": "group",
        "id": "gid",
        "label": "glabel",
        "format" : "horizontal",
        "repeatsMin":6,
        "repeatsMax":5,
        "repeatLabel":"repeatLabel",
        "repeatAddAnotherText":"repeatAddAnotherText",
        "fields": [
          {
            "type": "choice",
            "id": "cid",
            "label": "clabel",
            "choices": [
              "A",
              "B"
            ]
          }
        ],
        "presentationHint" : "collapseGroupUnderLabel"

      }
    """

    var jsr: JsResult[FieldValue] = null
    jsr = implicitly[Reads[FieldValue]].reads(Json.parse(jsonStr)); println(jsr); jsr should be(jsError)
    jsr = implicitly[Reads[FieldValue]].reads(Json.parse(jsonStr.replaceAll("6", """"A""""))); println(jsr); jsr should be(jsError)
    jsr = implicitly[Reads[FieldValue]].reads(Json.parse(jsonStr.replaceAll("6", "-1"))); println(jsr); jsr should be(jsError)
    jsr = implicitly[Reads[FieldValue]].reads(Json.parse(jsonStr.replaceAll("5", """"A""""))); println(jsr); jsr should be(jsError)

  }

}
