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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.data.NonEmptyList
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils._
import uk.gov.hmrc.gform.formtemplate.AcknowledgementSectionMaker

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
  case class NonRepeatingPage(page: Page) extends Section {
    override def title: SmartString = page.title
    override def expandedFormComponents: List[FormComponent] = page.expandedFormComponents
  }

  case class RepeatingPage(page: Page, repeats: Expr) extends Section {
    override def title: SmartString = page.title
    override def expandedFormComponents: List[FormComponent] = page.expandedFormComponents
  }

  case class AddToList(
    title: SmartString,
    description: SmartString,
    shortName: SmartString,
    summaryName: SmartString,
    includeIf: Option[IncludeIf],
    repeatsMax: Option[Expr],
    pages: NonEmptyList[Page],
    addAnotherQuestion: FormComponent,
    instruction: Option[Instruction],
    presentationHint: Option[PresentationHint],
    infoMessage: Option[SmartString]
  ) extends Section {
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
          LeafExpr(path + "description", a.description) ++
          LeafExpr(path + "shortName", a.shortName) ++
          LeafExpr(path + "summaryName", a.summaryName) ++
          LeafExpr(path + "includeIf", a.includeIf) ++
          LeafExpr(path + "repeatsMax", a.repeatsMax) ++
          LeafExpr(path + "pages", a.pages) ++
          LeafExpr(path + "addAnotherQuestion", a.addAnotherQuestion) ++
          LeafExpr(path + "instruction", a.instruction) ++
          LeafExpr(path + "infoMessage", a.infoMessage)
    }
}

case class DeclarationSection(
  title: SmartString,
  description: Option[SmartString],
  shortName: Option[SmartString],
  continueLabel: Option[SmartString],
  fields: List[FormComponent]
)

object DeclarationSection {
  implicit val format: OFormat[DeclarationSection] = Json.format[DeclarationSection]

  implicit val leafExprs: LeafExpr[DeclarationSection] = (path: TemplatePath, t: DeclarationSection) =>
    LeafExpr(path + "title", t.title) ++
      LeafExpr(path + "description", t.description) ++
      LeafExpr(path + "shortName", t.shortName) ++
      LeafExpr(path + "continueLabel", t.continueLabel) ++
      LeafExpr(path + "fields", t.fields)

}

case class AcknowledgementSection(
  title: SmartString,
  description: Option[SmartString],
  shortName: Option[SmartString],
  fields: List[FormComponent],
  showReference: Boolean,
  pdf: Option[AcknowledgementSectionPdf],
  instructionPdf: Option[AcknowledgementSectionPdf],
  displayFeedbackLink: Boolean
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
      LeafExpr(path + "instructionPdf", t.instructionPdf)

}

case class AcknowledgementSectionPdf(header: Option[SmartString], footer: Option[SmartString])

object AcknowledgementSectionPdf {
  implicit val format: OFormat[AcknowledgementSectionPdf] = Json.format[AcknowledgementSectionPdf]

  implicit val leafExprs: LeafExpr[AcknowledgementSectionPdf] = (path: TemplatePath, t: AcknowledgementSectionPdf) =>
    LeafExpr(path + "header", t.header) ++
      LeafExpr(path + "footer", t.footer)
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
