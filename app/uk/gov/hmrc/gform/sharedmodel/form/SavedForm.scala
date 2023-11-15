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

package uk.gov.hmrc.gform.sharedmodel.form

import play.api.libs.json.{ Format, JsString, JsValue, Json, OFormat, Reads, Writes }

import java.time.LocalDate
import java.time.format.DateTimeFormatter

final case class AllSavedVersions(stats: Seq[JsValue])

object AllSavedVersions {
  val empty = AllSavedVersions(Seq.empty[JsValue])
  implicit val format: OFormat[AllSavedVersions] = Json.format
}

final case class VersionStats(
  version: JsValue, // TODO change to Long, once we don't have version of type String in mongodb
  stats: List[CountData]
)

object VersionStats {
  implicit val format: OFormat[VersionStats] = Json.format
}

final case class CountData(
  count: Long,
  isEmail: Boolean
)

object CountData {
  implicit val format: OFormat[CountData] = Json.format
}

case class SavedFormDetail(
  createdDate: LocalDate,
  emailCount: Int,
  ggCount: Int
)

object SavedFormDetail {

  val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  val localDateReads = Reads.localDateReads("yyyy-MM-dd")
  val localDateWrites = Writes { date: LocalDate =>
    JsString(date.format(dateTimeFormatter))
  }

  implicit val localDateTimeFormat = Format(localDateReads, localDateWrites)

  implicit val format: OFormat[SavedFormDetail] = Json.format[SavedFormDetail]
}
