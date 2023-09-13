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

package uk.gov.hmrc.gform.history

import java.time.{ LocalDate, LocalDateTime }
import julienrf.json.derived
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters
import play.api.libs.json.{ Format, Json, OFormat }

sealed trait DateFilter extends Product with Serializable {
  val lte: Bson = this match {
    case DateFilter.DateOnly(localDate)     => Filters.lte("createdAt", localDate)
    case DateFilter.DateTime(localDateTime) => Filters.lte("createdAt", localDateTime)
  }

  val gte: Bson = this match {
    case DateFilter.DateOnly(localDate)     => Filters.gte("createdAt", localDate)
    case DateFilter.DateTime(localDateTime) => Filters.gte("createdAt", localDateTime)
  }
}

object DateFilter {
  final case class DateOnly(localDate: LocalDate) extends DateFilter
  final case class DateTime(localDateTime: LocalDateTime) extends DateFilter

  implicit val format: OFormat[DateFilter] = derived.oformat()
}

final case class HistoryFilter(
  from: Option[DateFilter],
  to: Option[DateFilter]
)

object HistoryFilter {
  implicit val format: Format[HistoryFilter] = Json.format[HistoryFilter]
}
