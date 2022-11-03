/*
 * Copyright 2022 HM Revenue & Customs
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
import play.api.libs.json.{ Format, JsString, OFormat }
import uk.gov.hmrc.gform.sharedmodel.ValueClassFormat
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats

import java.time.Instant
import java.util.UUID

final case class SdesSubmission(
  _id: CorrelationId,
  envelopeId: EnvelopeId,
  submittedAt: Instant = Instant.now,
  isProcessed: Boolean = false,
  confirmedAt: Option[Instant] = None
)

object SdesSubmission {
  def createSdesSubmission(envelopeId: EnvelopeId) =
    SdesSubmission(CorrelationId(UUID.randomUUID().toString), envelopeId)

  implicit val formatUUID: Format[UUID] =
    Format(_.validate[String].map(UUID.fromString), uuid => JsString(uuid.toString))

  implicit val format: OFormat[SdesSubmission] = {
    implicit val dtf: Format[Instant] = MongoJavatimeFormats.instantFormat
    implicit val envelopeIdFormat: Format[EnvelopeId] = EnvelopeId.vformat
    derived.oformat()
  }
}

final case class CorrelationId(value: String) extends AnyVal

object CorrelationId {
  implicit val mongoVformat: Format[CorrelationId] =
    ValueClassFormat.vformat("_id", CorrelationId.apply, x => JsString(x.value))
}
