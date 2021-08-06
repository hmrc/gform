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

package uk.gov.hmrc.gform.formtemplate

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

trait FormTemplateSupport {

  val addAnotherQuestion =
    mkFormComponent(
      "addAnother",
      Choice(YesNo, NonEmptyList.of(toSmartString("Yes"), toSmartString("No")), Horizontal, Nil, None, None),
      false
    )

  def mkFormTemplate(sections: List[Section.NonRepeatingPage]) = {
    val formTemplate = ExampleData.formTemplate.copy(sections = sections, emailParameters = None)
    formTemplate
  }

  def mkSectionNonRepeatingPage(formComponent: FormComponent): Section.NonRepeatingPage =
    mkSectionNonRepeatingPage(formComponents = List(formComponent))

  def mkSectionNonRepeatingPage(
    name: String = "Some Page",
    formComponents: List[FormComponent],
    instruction: Option[Instruction] = None,
    pageId: Option[PageId] = None
  ) =
    Section.NonRepeatingPage(
      Page(
        toSmartString(name),
        pageId,
        None,
        None,
        None,
        None,
        None,
        None,
        formComponents,
        None,
        None,
        instruction,
        None
      )
    )

  def mkFormComponent(id: String, expr: Expr) =
    FormComponent(
      FormComponentId(id),
      Text(ShortText.default, expr),
      toSmartString(id),
      None,
      None,
      None,
      None,
      true,
      false,
      true,
      false,
      false,
      None,
      None
    )

  def mkFormComponent(id: String, ct: ComponentType, editable: Boolean) =
    FormComponent(
      FormComponentId(id),
      ct,
      toSmartString(id),
      None,
      None,
      None,
      None,
      true,
      editable,
      true,
      false,
      false,
      None,
      None
    )

  def mkFormComponent(id: String, instruction: Option[Instruction] = None) =
    FormComponent(
      FormComponentId(id),
      Text(ShortText.default, Value),
      toSmartString(id),
      None,
      None,
      None,
      None,
      true,
      false,
      true,
      false,
      false,
      None,
      None,
      Nil,
      instruction
    )

  def mkFormComponentWithLabelSize(id: String, ct: ComponentType, labelSize: Option[LabelSize]) =
    FormComponent(
      FormComponentId(id),
      ct,
      toSmartString(id),
      None,
      None,
      None,
      None,
      true,
      true,
      true,
      false,
      false,
      None,
      None,
      Nil,
      None,
      labelSize
    )

  def mkSection(name: String, formComponents: List[FormComponent], instruction: Option[Instruction]) =
    Section.NonRepeatingPage(
      Page(
        toSmartString(name),
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        formComponents,
        None,
        None,
        instruction,
        None
      )
    )

  def mkSectionRepeatingPage(
    name: String = "Some Repeating Page",
    formComponents: List[FormComponent],
    pageId: Option[PageId] = None
  ) =
    Section.RepeatingPage(
      Page(
        toSmartString(name),
        pageId,
        None,
        None,
        None,
        None,
        None,
        None,
        formComponents,
        None,
        None,
        None,
        None
      ),
      Constant("2")
    )

  def mkAddToList(
    name: String,
    pages: NonEmptyList[Page],
    defaultPage: Option[Page] = None,
    addAnotherQuestion: FormComponent = addAnotherQuestion
  ) =
    Section.AddToList(
      toSmartString(name),
      Some(toSmartString(name)),
      toSmartString(name),
      toSmartString(name),
      toSmartString(name),
      None,
      None,
      pages,
      addAnotherQuestion,
      None,
      None,
      None,
      defaultPage
    )
}
