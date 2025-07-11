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

import java.time.{ Instant, LocalDateTime }
import julienrf.json.derived
import play.api.libs.json.Reads._
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.form.FormData
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateRaw

case class SnapshotOverview(
  templateId: FormTemplateId,
  originalTemplateId: FormTemplateId,
  snapshotId: SnapshotId,
  savedAt: Instant,
  description: Description,
  gformVersion: GformVersion,
  gformFrontendVersion: GformFrontendVersion,
  formData: Option[FormData],
  ggFormData: Option[GovernmentGatewayFormData]
)

object SnapshotOverview {

  def apply(snapshot: Snapshot, withData: Boolean): SnapshotOverview =
    SnapshotOverview(
      snapshot.snapshotTemplateId,
      snapshot.originalForm.formTemplateId,
      snapshot.snapshotId,
      snapshot.createdAt,
      snapshot.description,
      snapshot.gformVersion,
      snapshot.gformFrontendVersion,
      if (withData) Some(snapshot.originalForm.formData) else None,
      if (withData) snapshot.ggFormData else None
    )

  implicit val writes: OWrites[SnapshotOverview] = derived.owrites()
}

case class SaveRequest(
  formId: FormId,
  description: Description,
  gformFrontendVersion: GformFrontendVersion,
  ggFormData: Option[GovernmentGatewayFormData]
)

object SaveRequest {
  implicit val format: OFormat[SaveRequest] = derived.oformat()
}

case class SaveReply(
  formId: FormId
)

object SaveReply {
  implicit val format: OFormat[SaveReply] = derived.oformat()
}

case class UpdateSnapshotRequest(
  snapshotId: SnapshotId,
  formData: FormData,
  description: Description
)

object UpdateSnapshotRequest {
  implicit val format: OFormat[UpdateSnapshotRequest] = derived.oformat()
}

case class UpdateFormDataRequest(
  snapshotId: SnapshotId,
  formId: FormId
)

object UpdateFormDataRequest {
  implicit val format: OFormat[UpdateFormDataRequest] = derived.oformat()
}
case class SnapshotId(value: String) extends AnyVal

object SnapshotId {
  implicit val format: Format[SnapshotId] = Json.valueFormat
}

case class Description(value: String) extends AnyVal
object Description {
  implicit val format: Format[Description] = Json.valueFormat
}

case class GformVersion(value: String) extends AnyVal

object GformVersion {
  implicit val format: Format[GformVersion] = Json.valueFormat
}

case class GformFrontendVersion(value: String) extends AnyVal

object GformFrontendVersion {
  implicit val format: Format[GformFrontendVersion] = Json.valueFormat
}

case class Snapshot(
  snapshotId: SnapshotId,
  originalForm: Form,
  originalTemplate: FormTemplateRaw,
  description: Description,
  gformVersion: GformVersion,
  gformFrontendVersion: GformFrontendVersion,
  snapshotTemplateId: FormTemplateId,
  createdAt: Instant,
  ggFormData: Option[GovernmentGatewayFormData]
) {
  def toSnapshotForm(
    formTemplateId: FormTemplateId,
    currentForm: Form,
    maybeAccessCode: Option[String]
  ): Form = {
    val currentUserId = currentForm.userId
    val currentId = maybeAccessCode match {
      case Some(accessCode) => FormId.fromAccessCode(currentForm.userId, formTemplateId, AccessCode(accessCode))
      case _                => FormId(currentForm.userId, formTemplateId)
    }

    originalForm.copy(
      _id = currentId,
      userId = currentUserId,
      formTemplateId = formTemplateId
    )
  }

  def toSnapshotTemplate(): FormTemplateRaw =
    originalTemplate.copy(value = originalTemplate.value ++ Json.obj("_id" -> snapshotTemplateId.value))

  def updateWith(newFormData: FormData, newDescription: Description): Snapshot =
    copy(originalForm = originalForm.copy(formData = newFormData), description = newDescription)
}

object Snapshot {

  def apply(
    originalForm: Form,
    originalTemplate: FormTemplateRaw,
    description: Description,
    gformVersion: GformVersion,
    gformFrontendVersion: GformFrontendVersion,
    governmentGatewayFormData: Option[GovernmentGatewayFormData]
  ): Snapshot = {
    // add prefix to the template id for both saved form and template
    val snapshotId = SnapshotId(java.util.UUID.randomUUID().toString)
    val prefix = snapshotId.value.split("-").head
    val snapshotTemplateId = FormTemplateId(s"${prefix}_${originalForm.formTemplateId.value}")
    Snapshot(
      snapshotId,
      originalForm,
      originalTemplate,
      description,
      gformVersion,
      gformFrontendVersion,
      snapshotTemplateId,
      Instant.now,
      governmentGatewayFormData
    )
  }

  implicit val format: OFormat[Snapshot] = Json.format[Snapshot]
}

final case class SnapshotFilter(
  from: Option[LocalDateTime],
  to: Option[LocalDateTime],
  snapshotIdFilter: Option[String],
  descriptionFilter: Option[String],
  templateIdFilter: Option[String]
)

object SnapshotFilter {
  val empty: SnapshotFilter = SnapshotFilter(None, None, None, None, None)
  implicit val format: Format[SnapshotFilter] = Json.format[SnapshotFilter]
}
