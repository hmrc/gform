/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.config

import cats.Eq
import play.api.libs.json.{ Format, JsString }
import uk.gov.hmrc.gform.sharedmodel.ValueClassFormat
import uk.gov.hmrc.gform.sharedmodel.config.ContentType.{ `application/json`, `application/pdf`, `application/xml`, `application/zip`, `image/jpeg` }

case class ContentType(value: String) {
  def extension: String = this match {
    case `application/pdf`  => "pdf"
    case `application/xml`  => "xml"
    case `application/json` => "json"
    case `image/jpeg`       => "jpg"
    case `application/zip`  => "zip"
    case _                  => "pdf"
  }
}

object ContentType {

  val `application/pdf` = ContentType("application/pdf")
  val `application/xml` = ContentType("application/xml")
  val `application/json` = ContentType("application/json")
  val `image/jpeg` = ContentType("image/jpeg")
  val `text/xml` = ContentType("text/xml")
  val `application/zip` = ContentType("application/zip")

  /** .xls files
    */
  val `application/vnd.ms-excel` = ContentType("application/vnd.ms-excel")

  /** Excel2007 and above .xlsx files
    */
  val `application/vnd.openxmlformats-officedocument.spreadsheetml.sheet` = ContentType(
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  implicit val format: Format[ContentType] =
    ValueClassFormat.vformat("contentType", ContentType(_), x => JsString(x.value))

  implicit val equal: Eq[ContentType] = Eq.fromUniversalEquals
}
