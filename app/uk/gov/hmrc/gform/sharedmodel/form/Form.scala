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

package uk.gov.hmrc.gform.sharedmodel.form

import cats.Eq
import cats.implicits._
import julienrf.json.derived
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.{ UserId, ValueClassFormat }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, FormTemplateVersion, JsonUtils }

case class Form(
  _id: FormId,
  envelopeId: EnvelopeId,
  userId: UserId,
  formTemplateId: FormTemplateId,
  formTemplateVersion: Option[
    FormTemplateVersion
  ], // TODO Make this field mandatory once we have "mandatory version on template level" for some time
  formData: FormData,
  status: FormStatus,
  visitsIndex: VisitIndex,
  thirdPartyData: ThirdPartyData,
  envelopeExpiryDate: Option[EnvelopeExpiryDate],
  componentIdToFileId: FormComponentIdToFileIdMapping,
  taskIdTaskStatus: TaskIdTaskStatusMapping
)

object Form {
  private val thirdPartyData = "thirdPartyData"
  private val componentIdToFileId = "componentIdToFileId"
  private val formTemplateVersion = "version"
  private val taskIdTaskStatus = "taskIdTaskStatus"

  val readVisitIndex: Reads[VisitIndex] = VisitIndex.format

  private val thirdPartyDataWithFallback: Reads[ThirdPartyData] =
    (__ \ thirdPartyData)
      .readNullable[ThirdPartyData]
      .map(_.getOrElse(ThirdPartyData.empty))
      .orElse(JsonUtils.constReads(ThirdPartyData.empty))

  val formTemplateVersionWithFallback: Reads[Option[FormTemplateVersion]] =
    (__ \ formTemplateVersion)
      .read[Int]
      .map(v => Option(FormTemplateVersion(v)))
      .orElse(
        (__ \ formTemplateVersion)
          .readNullable[String]
          .map(_.map(v => FormTemplateVersion(v.toInt)))
          .orElse(JsonUtils.constReads(Option.empty[FormTemplateVersion]))
      )

  val componentIdToFileIdWithFallback: Reads[FormComponentIdToFileIdMapping] =
    (__ \ componentIdToFileId)
      .readNullable[FormComponentIdToFileIdMapping]
      .map(_.getOrElse(FormComponentIdToFileIdMapping.empty))
      .orElse(JsonUtils.constReads(FormComponentIdToFileIdMapping.empty))

  val taskIdTaskStatusWithFallback: Reads[TaskIdTaskStatusMapping] =
    (__ \ taskIdTaskStatus)
      .readNullable[TaskIdTaskStatusMapping]
      .map(_.getOrElse(TaskIdTaskStatusMapping.empty))
      .orElse(JsonUtils.constReads(TaskIdTaskStatusMapping.empty))

  private val reads: Reads[Form] = (
    (FormId.format: Reads[FormId]) and
      EnvelopeId.format and
      UserId.oformat and
      FormTemplateId.vformat and
      formTemplateVersionWithFallback and
      FormData.format and
      FormStatus.format and
      readVisitIndex and
      thirdPartyDataWithFallback and
      EnvelopeExpiryDate.optionFormat and
      componentIdToFileIdWithFallback and
      taskIdTaskStatusWithFallback
  )(Form.apply _)

  private val writes: OWrites[Form] = OWrites[Form](form =>
    FormId.format.writes(form._id) ++
      EnvelopeId.format.writes(form.envelopeId) ++
      UserId.oformat.writes(form.userId) ++
      FormTemplateId.oformat.writes(form.formTemplateId) ++
      form.formTemplateVersion.map(FormTemplateVersion.oformat.writes).getOrElse(Json.obj()) ++
      FormData.format.writes(form.formData) ++
      FormStatus.format.writes(form.status) ++
      VisitIndex.format.writes(form.visitsIndex) ++
      Json.obj(thirdPartyData -> ThirdPartyData.format.writes(form.thirdPartyData)) ++
      EnvelopeExpiryDate.optionFormat.writes(form.envelopeExpiryDate) ++
      Json.obj(componentIdToFileId -> FormComponentIdToFileIdMapping.format.writes(form.componentIdToFileId)) ++
      Json.obj(taskIdTaskStatus -> TaskIdTaskStatusMapping.format.writes(form.taskIdTaskStatus))
  )

  implicit val format: OFormat[Form] = OFormat[Form](reads, writes)

}

sealed trait FormStatus
case object InProgress extends FormStatus
case object Summary extends FormStatus
case object Validated extends FormStatus
case object Signed extends FormStatus
case object NeedsReview extends FormStatus
case object Accepting extends FormStatus
case object Returning extends FormStatus
case object Accepted extends FormStatus
case object Submitting extends FormStatus
case object Submitted extends FormStatus
case object Discarded extends FormStatus
case object ManuallySubmitted extends FormStatus

object FormStatus {
  implicit val equal: Eq[FormStatus] = Eq.fromUniversalEquals

  implicit val format: OFormat[FormStatus] = derived.oformat()

  val oformat: OFormat[FormStatus] = ValueClassFormat.validatedoformat(
    "status",
    statusStr =>
      FormStatus.unapply(statusStr) match {
        case Some(status) => JsSuccess(status)
        case None         => JsError(s"Invalid workflow state: $statusStr")
      },
    _.toString
  )

  val all: Set[FormStatus] =
    Set(
      InProgress,
      Summary,
      Validated,
      Signed,
      NeedsReview,
      Returning,
      Accepting,
      Accepted,
      Submitting,
      Submitted,
      Discarded,
      ManuallySubmitted
    )

  def unapply(s: String): Option[FormStatus] = all.find(_.toString === s)
}
