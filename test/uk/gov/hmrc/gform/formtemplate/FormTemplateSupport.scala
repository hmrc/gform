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

package uk.gov.hmrc.gform.formtemplate

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.SmartString

trait FormTemplateSupport {

  val yesNoLocalisedStrings =
    NonEmptyList.of(toSmartString("Yes"), toSmartString("No")).map(OptionData.IndexBased(_, None, None, None))

  val addAnotherQuestion =
    mkFormComponent(
      "addAnother",
      Choice(
        YesNo,
        yesNoLocalisedStrings,
        Horizontal,
        Nil,
        None,
        None,
        None,
        LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
        None,
        None,
        false
      ),
      false
    )

  def mkFormTemplate(sections: List[Section]) = {
    val formTemplate = ExampleData.formTemplate.copy(formKind = FormKind.Classic(sections), emailParameters = None)
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
        formComponents,
        None,
        None,
        instruction,
        None,
        None,
        None,
        None,
        None,
        None
      )
    )

  def mkFormComponent(id: String, expr: Expr) =
    FormComponent(
      FormComponentId(id),
      Text(ShortText.default, expr),
      toSmartString(id),
      false,
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
      false,
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

  def mkFormComponent(id: String, ct: ComponentType, label: SmartString) =
    FormComponent(
      FormComponentId(id),
      ct,
      label,
      false,
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
      None
    )

  def mkFormComponent(id: String, instruction: Option[Instruction] = None) =
    FormComponent(
      FormComponentId(id),
      Text(ShortText.default, Value),
      toSmartString(id),
      false,
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
      false,
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

  def mkFormComponentWithErrorMessage(id: String, errorMessage: Option[SmartString] = None) =
    FormComponent(
      FormComponentId(id),
      Text(ShortText.default, Value),
      toSmartString(id),
      false,
      None,
      None,
      None,
      None,
      true,
      false,
      true,
      false,
      false,
      errorMessage,
      None,
      Nil,
      None
    )

  def mkFormComponentWithNotPII(id: String, notPII: Boolean = false) =
    FormComponent(
      FormComponentId(id),
      Text(ShortText.default, Value),
      toSmartString(id),
      false,
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
      None,
      None,
      None,
      None,
      None,
      notPII
    )

  def mkFormComponentWithValidators(id: String, validators: List[FormComponentValidator] = Nil) =
    FormComponent(
      FormComponentId(id),
      Text(ShortText.default, Value),
      toSmartString(id),
      false,
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
      validators,
      None,
      None
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
        formComponents,
        None,
        None,
        instruction,
        None,
        None,
        None,
        None,
        None,
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
        formComponents,
        None,
        None,
        None,
        None,
        None,
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
    addAnotherQuestion: FormComponent = addAnotherQuestion,
    pageIdToDisplayAfterRemove: Option[PageId] = None
  ) =
    Section.AddToList(
      toSmartString(name),
      None,
      Some(toSmartString(name)),
      toSmartString(name),
      toSmartString(name),
      toSmartString(name),
      toSmartString(name),
      None,
      pages,
      None,
      None,
      None,
      addAnotherQuestion,
      None,
      None,
      None,
      None,
      defaultPage,
      pageIdToDisplayAfterRemove = pageIdToDisplayAfterRemove
    )
}
