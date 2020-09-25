/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.data.NonEmptyList
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils.nelFormat
import uk.gov.hmrc.gform.formtemplate.AcknowledgementSectionMaker

sealed trait Section extends Product with Serializable {
  def title(): SmartString
  def expandedFormComponents(): List[FormComponent]
}

object Section {
  case class NonRepeatingPage(page: Page) extends Section {
    override def title: SmartString = page.title
    override def expandedFormComponents: List[FormComponent] = page.expandedFormComponents
  }

  case class RepeatingPage(page: Page, repeats: TextExpression) extends Section {
    override def title: SmartString = page.title
    override def expandedFormComponents: List[FormComponent] = page.expandedFormComponents
  }

  case class AddToList(
    title: SmartString,
    description: Option[SmartString],
    shortName: Option[SmartString],
    includeIf: Option[IncludeIf],
    repeatsMax: Option[TextExpression],
    pages: NonEmptyList[Page])
      extends Section {
    override lazy val expandedFormComponents: List[FormComponent] = pages.toList.flatMap(_.expandedFormComponents)
  }

  implicit val format: OFormat[Section] = {
    implicit val derivedFormat: OFormat[Section] = derived.oformat()
    OFormatWithTemplateReadFallback(SectionTemplateReads.reads)
  }
}

case class DeclarationSection(
  title: SmartString,
  description: Option[SmartString],
  shortName: Option[SmartString],
  fields: List[FormComponent]
)

object DeclarationSection {
  implicit val format: OFormat[DeclarationSection] = Json.format[DeclarationSection]
}

case class AcknowledgementSection(
  title: SmartString,
  description: Option[SmartString],
  shortName: Option[SmartString],
  fields: List[FormComponent],
  showReference: Boolean,
  pdf: Option[AcknowledgementSectionPdf]
)

object AcknowledgementSection {

  private val templateReads: Reads[AcknowledgementSection] = Reads(
    json =>
      new AcknowledgementSectionMaker(json)
        .optAcknowledgementSection()
        .fold(us => JsError(us.toString), as => JsSuccess(as)))

  implicit val format: OFormat[AcknowledgementSection] = OFormatWithTemplateReadFallback(templateReads)
}

case class AcknowledgementSectionPdf(header: Option[SmartString], footer: Option[SmartString])

object AcknowledgementSectionPdf {
  implicit val format: OFormat[AcknowledgementSectionPdf] = Json.format[AcknowledgementSectionPdf]
}

case class EnrolmentSection(
  title: SmartString,
  shortName: Option[SmartString],
  fields: List[FormComponent],
  identifiers: NonEmptyList[IdentifierRecipe],
  verifiers: List[VerifierRecipe]
)

object EnrolmentSection {
  import JsonUtils._
  implicit val format: OFormat[EnrolmentSection] = Json.format[EnrolmentSection]
}

case class IdentifierRecipe(key: String, value: FormCtx)
object IdentifierRecipe {
  implicit val format: OFormat[IdentifierRecipe] = Json.format[IdentifierRecipe]
}

case class VerifierRecipe(key: String, value: FormCtx)
object VerifierRecipe {
  implicit val format: OFormat[VerifierRecipe] = Json.format[VerifierRecipe]
}
