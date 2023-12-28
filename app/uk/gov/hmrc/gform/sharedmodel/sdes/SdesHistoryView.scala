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

import julienrf.json.derived
import play.api.libs.json.{ Format, OFormat }
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId

import java.time.LocalDateTime

final case class SdesHistoryView(
  envelopeId: EnvelopeId,
  correlationId: CorrelationId,
  fileName: String,
  notifications: List[SdesHistoryNotification]
)
object SdesHistoryView {
  implicit val correlationIdFormat: Format[CorrelationId] = CorrelationId.mongoVformat
  implicit val envelopeIdFormat: Format[EnvelopeId] = EnvelopeId.vformat
  implicit val format: OFormat[SdesHistoryView] = derived.oformat()
}

final case class SdesHistoryNotification(
  notificationStatus: NotificationStatus,
  createdAt: LocalDateTime,
  failureReason: String,
  notifyRequest: Option[SdesNotifyRequest]
)

object SdesHistoryNotification {
  implicit val format: OFormat[SdesHistoryNotification] = derived.oformat()
}
