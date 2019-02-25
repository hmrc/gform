/*
 * Copyright 2019 HM Revenue & Customs
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

import julienrf.json.derived
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.{ ListAllInfo, UserId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, SectionNumber }

case class VisitIndex(visitsIndex: Set[Int]) extends AnyVal

object VisitIndex {

  val empty = VisitIndex(Set.empty)

  implicit val format: OFormat[VisitIndex] = Json.format

}

case class Form(
  _id: FormId,
  envelopeId: EnvelopeId,
  userId: UserId,
  formTemplateId: FormTemplateId,
  formData: FormData,
  status: FormStatus,
  visitsIndex: VisitIndex,
  envelopeExpiryDate: Option[EnvelopeExpiryDate],
  obligations: Option[ListAllInfo]
)

object Form {

  val readVisitIndex: Reads[VisitIndex] =
    (__ \ "visitsIndex").readNullable[List[Int]].map(a => VisitIndex(a.fold(Set.empty[Int])(_.toSet)))

  private val reads: Reads[Form] = ((FormId.format: Reads[FormId]) and
    EnvelopeId.format and
    UserId.oformat and
    FormTemplateId.vformat and
    FormData.format and
    FormStatus.format and
    readVisitIndex and
    EnvelopeExpiryDate.optionFormat and
    ListAllInfo.optionFormat)(Form.apply _)

  private val writes: OWrites[Form] = OWrites[Form](
    form =>
      FormId.format.writes(form._id) ++
        EnvelopeId.format.writes(form.envelopeId) ++
        UserId.oformat.writes(form.userId) ++
        FormTemplateId.oformat.writes(form.formTemplateId) ++
        FormData.format.writes(form.formData) ++
        FormStatus.format.writes(form.status) ++
        VisitIndex.format.writes(form.visitsIndex) ++
        EnvelopeExpiryDate.optionFormat.writes(form.envelopeExpiryDate) ++
        ListAllInfo.optionFormat.writes(form.obligations)
  )

  implicit val format: OFormat[Form] = OFormat[Form](reads, writes)

}

sealed trait FormStatus
case object InProgress extends FormStatus
case object Summary extends FormStatus
case object Validated extends FormStatus
case object Signed extends FormStatus
case object Submitted extends FormStatus

object FormStatus {
  implicit val format: OFormat[FormStatus] = derived.oformat
}
