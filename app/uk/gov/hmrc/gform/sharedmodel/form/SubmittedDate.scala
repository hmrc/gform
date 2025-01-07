/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.form

import play.api.libs.json.{ JsObject, JsSuccess, JsValue, Json, OFormat }

import java.time.LocalDateTime

case class SubmittedDate(submittedAt: LocalDateTime)

object SubmittedDate {

  implicit val format: OFormat[SubmittedDate] = Json.format[SubmittedDate]

  implicit val optionFormat: OFormat[Option[SubmittedDate]] = new OFormat[Option[SubmittedDate]] {
    override def writes(o: Option[SubmittedDate]): JsObject =
      o match {
        case Some(x) => Json.obj("submittedAt" -> Json.toJson(x.submittedAt))
        case None    => Json.obj()
      }

    override def reads(json: JsValue) =
      json.\("submittedAt").asOpt[LocalDateTime] match {
        case Some(x) => JsSuccess(Some(SubmittedDate(x)))
        case None    => JsSuccess(None)
      }
  }
}
