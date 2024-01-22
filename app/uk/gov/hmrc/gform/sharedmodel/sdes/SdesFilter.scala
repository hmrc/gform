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

package uk.gov.hmrc.gform.sharedmodel.sdes

import play.api.libs.json.{ Format, Json }
import uk.gov.hmrc.gform.history.DateFilter

final case class SdesFilter(
  page: Int,
  pageSize: Int,
  isProcessed: Option[Boolean],
  searchKey: Option[String],
  status: Option[NotificationStatus],
  destination: Option[SdesDestination],
  from: Option[DateFilter],
  to: Option[DateFilter]
)

object SdesFilter {
  implicit val format: Format[SdesFilter] = Json.format[SdesFilter]
}
