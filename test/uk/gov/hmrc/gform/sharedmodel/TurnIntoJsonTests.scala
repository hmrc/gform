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

//
//package uk.gov.hmrc.gform.models
//
//import play.api.data.validation.ValidationError
//import play.api.libs.json._
//import uk.gov.hmrc.gform.Spec
//
//class FieldValueGroupRawSpec extends Spec {
//
//  val gfvr = FieldValueRaw(
//    id = FieldId("groupid"),
//    `type` = Some(GroupRaw),
//    label = "Group Label",
//    format = Some(OrientationFormat("vertical"))
//  )
//
//  "A raw group" should "not parse if it has no fields specified" in {
//
//    val jsResult: JsResult[FieldValue] = gfvr.toFieldValue.reads(Json.obj())
//
//    jsResult shouldBe (jsError)
//
//    jsResult match {
//      case JsError(List((_, List(ValidationError(list, _*))))) => list shouldBe (List("Require 'fields' element in Group"))
//    }
//  }
//
//  it should "parse correctly if it has an empty list of fields" in {
//
//    gfvr.copy(fields = Some(Nil)).toFieldValue.reads(Json.obj()) should beJsSuccess(
//      FieldValue(FieldId("groupid"), Group(Nil, Vertical), "Group Label", None, None, true, true, true)
//    )
//  }
//
//  it should "parse a raw group with a non-mandatory raw text to a Group with a non-mandatory Text field" in {
//
//    val fvr = FieldValueRaw(
//      id = FieldId("regNum"),
//      `type` = Some(TextRaw),
//      mandatory = Some("false"),
//      submitMode = Some("standard"),
//      label = "String"
//    )
//
//    val res: JsResult[FieldValue] = gfvr.copy(fields = Some(List(fvr))).toFieldValue.reads(Json.obj())
//
//    res match {
//      case JsSuccess(FieldValue(_, Group(List(fv), _, _, _, _, _), _, _, _, _, _, _), _) => fv.mandatory shouldBe (false)
//      case JsSuccess(FieldValue(_, Group(list @ _, _, _, _, _, _), _, _, _, _, _, _), _) => fail(s"unexpected list ${list} in Group")
//      case JsError(s) => fail(s"expected successful parse but got ${s}")
//    }
//  }
//
//}
