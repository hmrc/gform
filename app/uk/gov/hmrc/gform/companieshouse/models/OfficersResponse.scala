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

package uk.gov.hmrc.gform.companieshouse.models

import play.api.libs.functional.syntax.{ toFunctionalBuilderOps, unlift }
import play.api.libs.json.{ Format, __ }

case class OfficersResponse(
  activeCount: Int,
  etag: String,
  items: List[Item],
  itemsPerPage: Int,
  kind: String,
  links: Links,
  resignedCount: Int,
  startIndex: Int,
  totalResults: Option[Int]
)

object OfficersResponse {
  implicit val format: Format[OfficersResponse] = (
    (__ \ "active_count").format[Int] and
      (__ \ "etag").format[String] and
      (__ \ "items").format[List[Item]] and
      (__ \ "items_per_page").format[Int] and
      (__ \ "kind").format[String] and
      (__ \ "links").format[Links] and
      (__ \ "resigned_count").format[Int] and
      (__ \ "start_index").format[Int] and
      (__ \ "total_results").formatNullable[Int]
  )(OfficersResponse.apply, unlift(OfficersResponse.unapply))
}
