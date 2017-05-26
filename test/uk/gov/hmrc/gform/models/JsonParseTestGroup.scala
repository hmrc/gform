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
import org.scalatest.{EitherValues, FlatSpec, Matchers}
import play.api.data.validation.ValidationError
import play.api.libs.json._

class FieldValueGroupSpec extends FlatSpec with Matchers with EitherValues with JsResultMatcher {

  val jsonStr =
    """
      |        {
      |          "type": "group",
      |          "id": "gid",
      |          "label": "glabel",
      |          "fields": [
      |            {
      |              "type": "choice",
      |              "id": "cid",
      |              "label": "clabel",
      |              "choices": [
      |                "A",
      |                "B"
      |              ]
      |            }
      |          ]
      |        }
    """.stripMargin

  "A raw group" should "not parse if it has no fields specified" in {

    val res = implicitly[Reads[FieldValue]].reads(Json.parse(jsonStr))

    println(res)

    res should beJsSuccess(FieldValue(FieldId("gid"),Group(List(FieldValue(FieldId("cid"),Choice(Radio,NonEmptyList.of("A", "B"),Vertical,List(),None),"clabel",None,true,true,true))),"glabel",None,true,true,true))

//    JsSuccess(FieldValue("gid",Group(List(FieldValue("cid",Choice(Radio,NonEmptyList("A", "B"),Vertical,List(),None),"clabel",None,true,true,true))),"glabel",None,true,true,true),"")


  }

}
