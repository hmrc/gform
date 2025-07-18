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
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, ExampleData, LangADT, LocalisedString, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

trait FormTemplateSupport {

  val yesNoLocalisedStrings =
    NonEmptyList.of(toSmartString("Yes"), toSmartString("No")).map(OptionData.IndexBased(_, None, None, None, None))

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
    pageId: Option[PageId] = None,
    dataRetrieve: Option[DataRetrieve] = None
  ) = {
    val dataRetrieves = dataRetrieve.map(dr => NonEmptyList.of(dr))
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
        dataRetrieves,
        None,
        None,
        None,
        None
      )
    )
  }

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

  def mkFormComponent(id: String, ct: ComponentType, pagesToRevisit: List[PageId]) =
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
      pageIdsToDisplayOnChange = Some(pagesToRevisit)
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
    pageIdToDisplayAfterRemove: Option[PageId] = None,
    repeatsUntil: Option[IncludeIf] = None,
    repeatsWhile: Option[IncludeIf] = None
  ) =
    Section.AddToList(
      toSmartString(name),
      None,
      Some(toSmartString(name)),
      AtlDescription.SmartStringBased(toSmartString(name)),
      toSmartString(name),
      toSmartString(name),
      toSmartString(name),
      None,
      pages,
      repeatsUntil,
      repeatsWhile,
      None,
      addAnotherQuestion,
      None,
      None,
      None,
      None,
      None,
      defaultPage,
      pageIdToDisplayAfterRemove = pageIdToDisplayAfterRemove,
      displayWidth = None
    )

  def mkInfoMessageComponent(
    id: String,
    infoText: String,
    summaryValue: Option[SmartString] = None,
    label: SmartString = SmartString(LocalisedString(Map()), List())
  ): FormComponent =
    FormComponent(
      FormComponentId(id),
      InformationMessage(
        NoFormat,
        toSmartString(infoText),
        summaryValue
      ),
      label,
      false,
      None,
      None,
      None,
      None,
      true,
      false,
      false,
      false,
      false,
      None,
      None
    )

}
