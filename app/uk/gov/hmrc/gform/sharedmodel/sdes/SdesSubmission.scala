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
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.sdes.NotificationStatus.FileReady
import uk.gov.hmrc.gform.sharedmodel.{ SubmissionRef, ValueClassFormat }
import uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats

import java.time.Instant
import java.util.UUID

final case class SdesSubmission(
  _id: CorrelationId,
  envelopeId: EnvelopeId,
  formTemplateId: FormTemplateId,
  submissionRef: SubmissionRef,
  submittedAt: Option[Instant] = None,
  isProcessed: Boolean = false,
  status: NotificationStatus,
  failureReason: Option[String] = None,
  confirmedAt: Option[Instant] = None,
  createdAt: Instant = Instant.now,
  lastUpdated: Option[Instant] = None,
  destination: Option[SdesDestination],
  isAlerted: Option[Boolean] = None,
  lockedAt: Option[Instant] = None
) {
  val sdesDestination: SdesDestination = destination.getOrElse(SdesDestination.Dms)
}

object SdesSubmission {
  def createSdesSubmission(
    correlationId: CorrelationId,
    envelopeId: EnvelopeId,
    formTemplateId: FormTemplateId,
    submissionRef: SubmissionRef,
    destination: SdesDestination
  ) =
    SdesSubmission(
      correlationId,
      envelopeId,
      formTemplateId,
      submissionRef,
      status = FileReady,
      destination = Some(destination)
    )

  implicit val formatUUID: Format[UUID] =
    Format(_.validate[String].map(UUID.fromString), uuid => JsString(uuid.toString))

  implicit val format: OFormat[SdesSubmission] = {
    implicit val dtf: Format[Instant] = MongoJavatimeFormats.instantFormat
    implicit val correlationIdFormat: Format[CorrelationId] = CorrelationId.mongoVformat
    implicit val envelopeIdFormat: Format[EnvelopeId] = EnvelopeId.vformat
    implicit val formTemplateIdFormat: Format[FormTemplateId] = FormTemplateId.vformat
    implicit val submissionRefFormat: Format[SubmissionRef] = SubmissionRef.vformat
    derived.oformat()
  }
}

final case class CorrelationId(value: String) extends AnyVal

object CorrelationId {
  val mongoVformat: Format[CorrelationId] =
    ValueClassFormat.vformat("_id", CorrelationId.apply, x => JsString(x.value))
  val vformat: Format[CorrelationId] =
    ValueClassFormat.vformat("correlationId", CorrelationId.apply, x => JsString(x.value))
}
