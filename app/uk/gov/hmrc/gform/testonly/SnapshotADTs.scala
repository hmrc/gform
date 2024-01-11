/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.testonly

import uk.gov.hmrc.mongo.cache.CacheItem
import java.time.Instant
import julienrf.json.derived
import play.api.libs.json.Reads._
import play.api.libs.json._

case class Snapshot(
  templateId: String,
  snapshotId: String,
  savedAt: Instant,
  description: String
)

object Snapshot {

  def apply(cacheItem: CacheItem): Snapshot = {
    val templateId = cacheItem.data.validate((__ \ "form" \ "formTemplateId").json.pick[JsString]).get.value
    val snapshotId = cacheItem.id
    val savedAt = cacheItem.createdAt
    val description = cacheItem.data.validate((__ \ "description").json.pick[JsString]).get.value
    Snapshot(
      templateId,
      snapshotId,
      savedAt,
      description
    )
  }

  implicit val writes: OWrites[Snapshot] = derived.owrites()

}

case class SnapshotWithData(
  snapshot: Snapshot,
  formData: JsObject
)
object SnapshotWithData {
  implicit val writes: OWrites[SnapshotWithData] = derived.owrites()
}

case class SaveRequest(
  formId: String,
  description: String
)

object SaveRequest {
  implicit val format: OFormat[SaveRequest] = derived.oformat()
}

case class SaveReply(
  snapshotId: String
)

object SaveReply {
  implicit val format: OFormat[SaveReply] = derived.oformat()
}

case class UpdateSnapshotRequest(
  snapshotId: String,
  formData: JsObject,
  description: String
)

object UpdateSnapshotRequest {
  implicit val format: OFormat[UpdateSnapshotRequest] = derived.oformat()
}
