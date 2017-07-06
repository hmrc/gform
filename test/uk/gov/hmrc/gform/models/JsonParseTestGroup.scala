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
        ]
      }
    """

  "A raw group" should "parse" in {

    val jsResult = implicitly[Reads[FieldValue]].reads(Json.parse(jsonStr))

    jsResult should beJsSuccess(FieldValue(
      FieldId("gid"),
      Group(
        List(
          FieldValue(
            FieldId("cid"),
            Choice(Radio, NonEmptyList.of("A", "B"), Vertical, List(), None), "clabel", None, None, true, true, true
          )
        ),
        Horizontal,
        Some(5), Some(1), Some("repeatLabel"), Some("repeatAddAnotherText")
      ),
      "glabel", None, None, true, true, true
    ))

  }

}
