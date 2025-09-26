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
import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, SmartString }
import uk.gov.hmrc.gform.ops.FormComponentOps
import uk.gov.hmrc.gform.sharedmodel.formtemplate.LayoutDisplayWidth.LayoutDisplayWidth

case class Page(
  title: SmartString,
  id: Option[PageId],
  noPIITitle: Option[SmartString],
  description: Option[SmartString],
  shortName: Option[SmartString],
  caption: Option[SmartString] = None,
  includeIf: Option[IncludeIf],
  fields: List[FormComponent],
  continueLabel: Option[SmartString],
  continueIf: Option[ContinueIf],
  instruction: Option[Instruction],
  presentationHint: Option[PresentationHint],
  dataRetrieve: Option[NonEmptyList[DataRetrieve]],
  confirmation: Option[Confirmation],
  redirects: Option[NonEmptyList[RedirectCtx]],
  hideSaveAndComeBackButton: Option[Boolean],
  removeItemIf: Option[RemoveItemIf],
  displayWidth: Option[LayoutDisplayWidth] = None,
  specimenNote: Option[String] = None
) {
  lazy val expandedFormComponents: List[FormComponent] = fields.flatMap(_.expandedFormComponents)

  def formComponents[A](predicate: PartialFunction[FormComponent, A]): List[A] = allFormComponents.collect(predicate)

  val allFormComponents: List[FormComponent] = fields.flatMap {
    case IsGroup(group)                          => group.fields
    case fc @ IsRevealingChoice(revealingChoice) => fc :: revealingChoice.options.toList.flatMap(_.revealingFields)
    case otherwise                               => otherwise :: Nil
  }
  val allFormComponentIds: List[FormComponentId] = (fields ++ fields
    .flatMap {
      case IsGroup(group)                     => group.fields
      case IsRevealingChoice(revealingChoice) => revealingChoice.options.toList.flatMap(_.revealingFields)
      case otherwise                          => Nil
    } ++ confirmation.toList.map(_.question))
    .map(_.id)

  val numericFields: List[FormComponentId] = allFormComponents.filter(_.isNumeric).map(_.id)

  def dataRetrieves(): List[DataRetrieve] = dataRetrieve.toList.flatMap(_.toList)
}
object Page {

  import JsonUtils._
  implicit val pageFormat: OFormat[Page] = derived.oformat()

  implicit val leafExprs: LeafExpr[Page] = (path: TemplatePath, t: Page) =>
    leafExprsNoFields.exprs(path, t) ++
      LeafExpr(path + "fields", t.fields)

  val leafExprsNoFields: LeafExpr[Page] = (path: TemplatePath, t: Page) =>
    LeafExpr(path + "title", t.title) ++
      LeafExpr(path + "description", t.description) ++
      LeafExpr(path + "shortName", t.shortName) ++
      LeafExpr(path + "caption", t.caption) ++
      LeafExpr(path + "continueLabel", t.continueLabel) ++
      LeafExpr(path + "includeIf", t.includeIf) ++
      LeafExpr(path + "instruction", t.instruction) ++
      LeafExpr(path + "confirmation", t.confirmation) ++
      LeafExpr(path + "redirects", t.redirects) ++
      LeafExpr(path + "dataRetrieve", t.dataRetrieve) ++
      LeafExpr(path + "removeItemIf", t.removeItemIf)

}
