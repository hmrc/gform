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

package uk.gov.hmrc.gform.sharedmodel.form

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import play.api.libs.json._

case class EnvelopeExpiryDate(ldt: LocalDateTime)

object EnvelopeExpiryDate {

  private val dateTimeFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'")
  implicit val localDateTimeFormat = new Format[LocalDateTime] {
    override def reads(json: JsValue): JsResult[LocalDateTime] = json match {
      case JsString(s) => JsSuccess(LocalDateTime.from(dateTimeFormat.parse(s)))
      case v           => JsError(s"Expected a date time of the form yyyy-MM-dd'T'HH:mm:ss'Z'. Got $v")
    }

    override def writes(o: LocalDateTime): JsValue = JsString(dateTimeFormat.format(o))
  }

  implicit val format: OFormat[EnvelopeExpiryDate] = Json.format[EnvelopeExpiryDate]

  implicit val optionFormat: OFormat[Option[EnvelopeExpiryDate]] = new OFormat[Option[EnvelopeExpiryDate]] {
    override def writes(o: Option[EnvelopeExpiryDate]): JsObject =
      o match {
        case Some(x) => Json.obj("ldt" -> Json.toJson(x.ldt))
        case None    => Json.obj()
      }

    override def reads(json: JsValue) =
      json.\("ldt").asOpt[LocalDateTime] match {
        case Some(x) => JsSuccess(Some(EnvelopeExpiryDate(x)))
        case None    => JsSuccess(None)
      }
  }
}
