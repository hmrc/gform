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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.data.NonEmptyList
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils._
import uk.gov.hmrc.gform.formtemplate.AcknowledgementSectionMaker
import uk.gov.hmrc.gform.sharedmodel.formtemplate.LayoutDisplayWidth.LayoutDisplayWidth

sealed trait Section extends Product with Serializable {
  def title(): SmartString
  def expandedFormComponents(): List[FormComponent]

  def formComponents[A](predicate: PartialFunction[FormComponent, A]): List[A] = fold(_.page.formComponents(predicate))(
    _.page.formComponents(predicate)
  )(_.pages.toList.flatMap(_.formComponents(predicate)))

  def fold[B](f: Section.NonRepeatingPage => B)(g: Section.RepeatingPage => B)(h: Section.AddToList => B): B =
    this match {
      case n: Section.NonRepeatingPage => f(n)
      case r: Section.RepeatingPage    => g(r)
      case a: Section.AddToList        => h(a)
    }
}

object Section {
  final case class NonRepeatingPage(page: Page) extends Section {
    override def title(): SmartString = page.title
    override def expandedFormComponents(): List[FormComponent] = page.expandedFormComponents
  }

  final case class RepeatingPage(page: Page, repeats: Expr) extends Section {
    override def title(): SmartString = page.title
    override def expandedFormComponents(): List[FormComponent] = page.expandedFormComponents
  }

  final case class AddToList(
    title: SmartString,
    caption: Option[SmartString],
    noPIITitle: Option[SmartString],
    description: AtlDescription,
    summaryDescription: SmartString,
    shortName: SmartString,
    summaryName: SmartString,
    includeIf: Option[IncludeIf],
    pages: NonEmptyList[Page],
    repeatsUntil: Option[IncludeIf],
    repeatsWhile: Option[IncludeIf],
    repeaterContinueLabel: Option[SmartString],
    addAnotherQuestion: FormComponent,
    instruction: Option[Instruction],
    presentationHint: Option[PresentationHint],
    infoMessage: Option[SmartString],
    errorMessage: Option[SmartString],
    descriptionTotal: Option[AtlDescription.KeyValueBased],
    defaultPage: Option[Page] = None,
    cyaPage: Option[CheckYourAnswersPage] = None,
    fields: Option[NonEmptyList[FormComponent]] = None,
    pageIdToDisplayAfterRemove: Option[PageId] = None,
    declarationSection: Option[DeclarationSection] = None,
    displayWidth: Option[LayoutDisplayWidth]
  ) extends Section {
    val pageId: PageId = PageId(addAnotherQuestion.id.value)
    override lazy val expandedFormComponents: List[FormComponent] = pages.toList.flatMap(_.expandedFormComponents)
  }

  implicit val format: OFormat[Section] =
    OFormatWithTemplateReadFallback(SectionTemplateReads.reads)

  implicit val leafExprs: LeafExpr[Section] = (path: TemplatePath, t: Section) =>
    t match {
      case n: Section.NonRepeatingPage => LeafExpr(path, n.page)
      case r: Section.RepeatingPage    => ExprWithPath(path, r.repeats) :: LeafExpr(path, r.page)
      case a: Section.AddToList =>
        LeafExpr(path + "title", a.title) ++
          LeafExpr(path + "caption", a.caption) ++
          LeafExpr(path + "noPIITitle", a.noPIITitle) ++
          LeafExpr(path + "description", a.description) ++
          LeafExpr(path + "summaryDescription", a.summaryDescription) ++
          LeafExpr(path + "shortName", a.shortName) ++
          LeafExpr(path + "summaryName", a.summaryName) ++
          LeafExpr(path + "includeIf", a.includeIf) ++
          LeafExpr(path + "pages", a.pages) ++
          LeafExpr(path + "repeatsUntil", a.repeatsUntil) ++
          LeafExpr(path + "repeatsWhile", a.repeatsWhile) ++
          LeafExpr(path + "repeaterContinueLabel", a.repeaterContinueLabel) ++
          LeafExpr(path + "addAnotherQuestion", a.addAnotherQuestion) ++
          LeafExpr(path + "instruction", a.instruction) ++
          LeafExpr(path + "infoMessage", a.infoMessage) ++
          LeafExpr(path + "defaultPage", a.defaultPage) ++
          LeafExpr(path + "cyaPage", a.cyaPage) ++
          LeafExpr(path + "fields", a.fields) ++
          LeafExpr(path + "errorMessage", a.infoMessage) ++
          LeafExpr(path + "descriptionTotal", a.descriptionTotal) ++
          LeafExpr(path + "declarationSection", a.declarationSection)
    }
}

sealed trait AtlDescription extends Product with Serializable

object AtlDescription {
  case class SmartStringBased(value: SmartString) extends AtlDescription
  case class KeyValueBased(key: SmartString, value: SmartString) extends AtlDescription

  implicit val formatKeyValuePair: OFormat[KeyValueBased] = Json.format[KeyValueBased]
  def reads: Reads[AtlDescription] = Reads { json =>
    (json \ "key") match {
      case JsDefined(JsArray(_)) => json.validate[KeyValueBased]
      case _                     => json.validate[SmartString].flatMap(s => JsSuccess(SmartStringBased(s)))
    }
  }

  implicit val format: OFormat[AtlDescription] = OFormatWithTemplateReadFallback(reads)

  implicit val leafExprs: LeafExpr[AtlDescription] = (path: TemplatePath, t: AtlDescription) =>
    t match {
      case s: SmartStringBased => LeafExpr(path, s.value)
      case KeyValueBased(key, value) =>
        LeafExpr(path + "key", key) ++ LeafExpr(path + "value", value)
    }

}

case class DeclarationSection(
  title: SmartString,
  caption: Option[SmartString],
  noPIITitle: Option[SmartString],
  description: Option[SmartString],
  shortName: Option[SmartString],
  continueLabel: Option[SmartString],
  fields: List[FormComponent],
  includeIf: Option[IncludeIf]
)

object DeclarationSection {
  implicit val format: OFormat[DeclarationSection] = Json.format[DeclarationSection]

  implicit val leafExprs: LeafExpr[DeclarationSection] = (path: TemplatePath, t: DeclarationSection) =>
    LeafExpr(path + "title", t.title) ++
      LeafExpr(path + "caption", t.caption) ++
      LeafExpr(path + "description", t.description) ++
      LeafExpr(path + "shortName", t.shortName) ++
      LeafExpr(path + "continueLabel", t.continueLabel) ++
      LeafExpr(path + "fields", t.fields) ++
      LeafExpr(path + "includeIf", t.includeIf)

}

case class AcknowledgementSection(
  title: Option[SmartString],
  description: Option[SmartString],
  shortName: Option[SmartString],
  fields: List[FormComponent],
  showReference: Boolean,
  pdf: Option[PdfCtx],
  instructionPdf: Option[PdfCtx],
  displayFeedbackLink: Boolean,
  noPIITitle: Option[SmartString],
  showBanner: Boolean
)

object AcknowledgementSection {

  private val templateReads: Reads[AcknowledgementSection] = Reads(json =>
    new AcknowledgementSectionMaker(json)
      .optAcknowledgementSection()
      .fold(us => JsError(us.toString), as => JsSuccess(as))
  )

  implicit val format: OFormat[AcknowledgementSection] = OFormatWithTemplateReadFallback(templateReads)

  implicit val leafExprs: LeafExpr[AcknowledgementSection] = (path: TemplatePath, t: AcknowledgementSection) =>
    LeafExpr(path + "title", t.title) ++
      LeafExpr(path + "description", t.description) ++
      LeafExpr(path + "shortName", t.shortName) ++
      LeafExpr(path + "fields", t.fields) ++
      LeafExpr(path + "pdf", t.pdf) ++
      LeafExpr(path + "instructionPdf", t.instructionPdf) ++
      LeafExpr(path + "noPIITitle", t.noPIITitle)
}

case class EnrolmentSection(
  title: SmartString,
  noPIITitle: Option[SmartString],
  shortName: Option[SmartString],
  fields: List[FormComponent],
  identifiers: NonEmptyList[IdentifierRecipe],
  verifiers: List[VerifierRecipe],
  continueLabel: Option[SmartString]
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

final case class EnrolmentOutcomes(
  notMatchedPage: EnrolmentOutcome,
  alreadyLinkedPage: EnrolmentOutcome,
  technicalFailurePage: EnrolmentOutcome,
  successPage: EnrolmentOutcome,
  insufficientCredentialsPage: EnrolmentOutcome
)

object EnrolmentOutcomes {
  implicit val format: OFormat[EnrolmentOutcomes] = Json.format[EnrolmentOutcomes]
}

final case class EnrolmentOutcome(
  title: SmartString,
  content: SmartString
)

object EnrolmentOutcome {
  implicit val format: OFormat[EnrolmentOutcome] = Json.format[EnrolmentOutcome]
}
