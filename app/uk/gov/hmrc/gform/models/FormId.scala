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

case class FormId(value: String)

object FormId {

  def apply(userId: UserId, formTypeId: FormTypeId): FormId =
    new FormId(s"""${userId.value}-${formTypeId.value}""")

  implicit val format: OFormat[FormId] = OFormat[FormId](reads, writes)

  private lazy val writes: OWrites[FormId] = OWrites[FormId](id => Json.obj("_id" -> id.value))
  private lazy val reads: Reads[FormId] = Reads[FormId] { (jsObj: JsValue) =>
    (jsObj \ "_id") match {
      case JsDefined(JsString(id)) => JsSuccess(FormId(id))
      case _ => jsObj match {
        case JsString(x) => JsSuccess(FormId(x))
        case _ => JsError(s"Invalid formId, expected fieldName '_id', got: $jsObj")
      }
    }
  }

}
