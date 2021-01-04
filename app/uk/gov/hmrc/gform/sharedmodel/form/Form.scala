/*
 * Copyright 2021 HM Revenue & Customs
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
import cats.instances.string._
import cats.syntax.eq._
import julienrf.json.derived
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId, JsonUtils }

import scala.util.Try

case class VisitIndex(visitsIndex: Set[Int]) extends AnyVal

object VisitIndex {

  val key: String = "_visits_"

  val formComponentId: FormComponentId = FormComponentId(key)

  val empty: VisitIndex = VisitIndex(Set.empty)

  implicit val format: OFormat[VisitIndex] = Json.format

  def fromStrings(s: Seq[String]): VisitIndex =
    if (s.isEmpty)
      VisitIndex.empty
    else
      VisitIndex(s.flatMap { indexAsString =>
        Try(indexAsString.toInt).toOption.fold(List.empty[Int])(_ :: Nil)
      }.toSet)
}

case class Form(
  _id: FormId,
  envelopeId: EnvelopeId,
  userId: UserId,
  formTemplateId: FormTemplateId,
  formData: FormData,
  status: FormStatus,
  visitsIndex: VisitIndex,
  thirdPartyData: ThirdPartyData,
  envelopeExpiryDate: Option[EnvelopeExpiryDate] = None
)

object Form {
  private val readVisitIndex: Reads[VisitIndex] =
    (__ \ "visitsIndex").readNullable[List[Int]].map(a => VisitIndex(a.fold(Set.empty[Int])(_.toSet)))

  private val thirdPartyDataWithFallback: Reads[ThirdPartyData] =
    (__ \ "thirdPartyData")
      .readNullable[ThirdPartyData]
      .map(_.getOrElse(ThirdPartyData.empty))
      .orElse(JsonUtils.constReads(ThirdPartyData.empty))

  private val reads: Reads[Form] = (
    (FormId.format: Reads[FormId]) and
      EnvelopeId.format and
      UserId.oformat and
      FormTemplateId.vformat and
      FormData.format and
      FormStatus.format and
      readVisitIndex and
      thirdPartyDataWithFallback and
      EnvelopeExpiryDate.optionFormat
  )(Form.apply _)

  private val writes: OWrites[Form] = OWrites[Form](
    form =>
      FormId.format.writes(form._id) ++
        EnvelopeId.format.writes(form.envelopeId) ++
        UserId.oformat.writes(form.userId) ++
        FormTemplateId.oformat.writes(form.formTemplateId) ++
        FormData.format.writes(form.formData) ++
        FormStatus.format.writes(form.status) ++
        VisitIndex.format.writes(form.visitsIndex) ++
        Json.obj("thirdPartyData" -> ThirdPartyData.format.writes(form.thirdPartyData)) ++
        EnvelopeExpiryDate.optionFormat.writes(form.envelopeExpiryDate)
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
      ManuallySubmitted)

  def unapply(s: String): Option[FormStatus] = all.find(_.toString === s)
}
