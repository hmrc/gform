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

object ValueClassFormat {

  def oformat[A](fieldName: String, read: String => A, write: A => String): OFormat[A] =
    OFormat[A](
      Reads[A] {
        case JsString(str) => JsSuccess(read(str))
        case JsObject(x) =>
          x.get(fieldName) match {
            case Some(JsString(str)) => JsSuccess(read(str))
            case _                   => JsError(s"Expected $fieldName field")
          }
        case other => JsError(s"Invalid json, not found '$fieldName'")
      },
      OWrites[A](a => Json.obj(fieldName -> write(a)))
    )

  def vformat[A](fieldName: String, read: String => A, write: A => JsValue): Format[A] =
    Format[A](
      Reads[A] {
        case JsString(str) => JsSuccess(read(str))
        case JsObject(x) =>
          x.get(fieldName) match {
            case Some(JsString(str)) => JsSuccess(read(str))
            case _                   => JsError(s"Expected $fieldName field")
          }
        case other => JsError(s"Invalid json, not found '$fieldName'")
      },
      Writes[A](a => write(a))
    )

  def simpleFormat[A: Format](fromStringToA: String => A)(fromAToString: A => String) =
    Format[A](
      Reads[A] {
        case JsString(str) => JsSuccess(fromStringToA(str))
        case unknown       => JsError(s"JsString value expected, got: $unknown")
      },
      Writes[A](a => JsString(fromAToString(a)))
    )

}
