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
import play.api.libs.json.{ Format, OFormat, Reads, Writes }
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId

import java.time.LocalDateTime
import java.util.UUID

final case class SdesHistory(
  _id: UUID,
  createdAt: LocalDateTime,
  notificationStatus: NotificationStatus,
  envelopeId: EnvelopeId,
  correlationId: CorrelationId,
  fileName: String,
  failureReason: String,
  notifyRequest: Option[SdesNotifyRequest]
)

object SdesHistory {

  def create(
    envelopeId: EnvelopeId,
    correlationId: CorrelationId,
    notificationStatus: NotificationStatus,
    fileName: String,
    failureReason: Option[String],
    notifyRequest: Option[SdesNotifyRequest]
  ): SdesHistory =
    SdesHistory(
      UUID.randomUUID,
      LocalDateTime.now(),
      notificationStatus,
      envelopeId,
      correlationId,
      fileName,
      failureReason.getOrElse(""),
      notifyRequest
    )

  implicit val format: OFormat[SdesHistory] = {
    implicit val dtf: Format[LocalDateTime] = Format(Reads.DefaultLocalDateTimeReads, Writes.DefaultLocalDateTimeWrites)
    implicit val correlationIdFormat: Format[CorrelationId] = CorrelationId.mongoVformat
    implicit val envelopeIdFormat: Format[EnvelopeId] = EnvelopeId.vformat
    implicit val notificationStatusFormat: Format[NotificationStatus] = NotificationStatus.format
    derived.oformat()
  }
}
