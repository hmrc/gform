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

import java.time.Instant
import julienrf.json.derived
import play.api.libs.json.Reads._
import play.api.libs.json._
import uk.gov.hmrc.crypto.{ Decrypter, Encrypter }
import uk.gov.hmrc.gform.save4later.EncryptedFormFormat
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.form.FormData
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateRaw

case class Snapshot(
  templateId: FormTemplateId,
  snapshotId: SnapshotId,
  savedAt: Instant,
  description: Description,
  gformVersion: GformVersion,
  gformFrontendVersion: GformFrontendVersion
)

object Snapshot {

  def apply(snapshotItem: SnapshotItem): Snapshot =
    Snapshot(
      snapshotItem.snapshotTemplateId,
      snapshotItem.snapshotId,
      snapshotItem.createdAt,
      snapshotItem.description,
      snapshotItem.gformVersion,
      snapshotItem.gformFrontendVersion
    )

  implicit val writes: OWrites[Snapshot] = derived.owrites()
}

case class SnapshotWithData(
  snapshot: Snapshot,
  formData: FormData
)
object SnapshotWithData {
  def apply(snapshotItem: SnapshotItem): SnapshotWithData =
    SnapshotWithData(Snapshot(snapshotItem), snapshotItem.originalForm.formData)
  implicit val writes: OWrites[SnapshotWithData] = derived.owrites()
}

case class SaveRequest(
  formId: FormId,
  description: Description,
  gformFrontendVersion: GformFrontendVersion
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
  formData: JsObject,
  description: Description
)

object UpdateSnapshotRequest {
  implicit val format: OFormat[UpdateSnapshotRequest] = derived.oformat()
}

case class UpdateFormDataRequest(
  formId: FormId,
  formData: JsObject
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

case class SnapshotItem(
  snapshotId: SnapshotId,
  originalForm: Form,
  originalTemplate: FormTemplateRaw,
  description: Description,
  gformVersion: GformVersion,
  gformFrontendVersion: GformFrontendVersion,
  snapshotTemplateId: FormTemplateId,
  createdAt: Instant
) {
  def toSnapshotForm(currentForm: Form): Form = {
    val currentUserId = currentForm.userId
    val currentId = currentForm._id
    originalForm.copy(
      _id = currentId,
      userId = currentUserId,
      formTemplateId = snapshotTemplateId
    )
  }

  def toSnapshotTemplate(): FormTemplateRaw =
    originalTemplate.copy(value = originalTemplate.value ++ Json.obj("_id" -> snapshotTemplateId.value))

  def updateWith(newFormData: FormData, newDescription: Description): SnapshotItem =
    copy(originalForm = originalForm.copy(formData = newFormData), description = newDescription)
}

object SnapshotItem {

  def apply(
    originalForm: Form,
    originalTemplate: FormTemplateRaw,
    description: Description,
    gformVersion: GformVersion,
    gformFrontendVersion: GformFrontendVersion
  ): SnapshotItem = {
    // add prefix to the template id for both saved form and template
    val prefix = UniqueStringGenerator.generateUniqueString()
    val snapshotTemplateId = FormTemplateId(s"${prefix}_${originalForm.formTemplateId.value}")
    val snapshotId = SnapshotId(java.util.UUID.randomUUID().toString)
    SnapshotItem(
      snapshotId,
      originalForm,
      originalTemplate,
      description,
      gformVersion,
      gformFrontendVersion,
      snapshotTemplateId,
      Instant.now
    )
  }

  def format(jsonCrypto: Encrypter with Decrypter): Format[SnapshotItem] = {
    implicit val formatFormEncrypted: Format[Form] = EncryptedFormFormat.formatEncrypted(jsonCrypto)
    implicit val instantFormat = uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats.instantFormat
    Json.format[SnapshotItem]
  }
}
