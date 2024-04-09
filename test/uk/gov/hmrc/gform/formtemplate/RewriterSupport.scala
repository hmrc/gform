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
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.SmartString

trait RewriterSupport {

  val simplePage = Page(
    title = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "Default title")), Nil),
    caption = None,
    id = None,
    noPIITitle = None,
    description = None,
    shortName = None,
    includeIf = None,
    fields = Nil,
    continueLabel = None,
    continueIf = None,
    instruction = None,
    presentationHint = None,
    dataRetrieve = None,
    confirmation = None,
    redirects = None,
    hideSaveAndComeBackButton = None,
    removeItemIf = None,
    displayWidth = None
  )

  val simpleFormComponent = FormComponent(
    id = FormComponentId("Default form component ID"),
    `type` = PostcodeLookup(None, None, None),
    label = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "Default label")), Nil),
    helpText = None,
    shortName = None,
    includeIf = None,
    validIf = None,
    mandatory = true,
    editable = true,
    submissible = true,
    derived = true,
    errorMessage = None,
    presentationHint = None,
    instruction = None,
    labelSize = None,
    errorShortName = None,
    errorShortNameStart = None,
    errorExample = None,
    extraLetterSpacing = None
  )

  def mkDefaultAtl(pages: NonEmptyList[Page]) = Section.AddToList(
    title = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "Default title")), Nil),
    caption = None,
    noPIITitle = None,
    description = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "Default description")), Nil),
    summaryDescription =
      SmartString(localised = LocalisedString(m = Map(LangADT.En -> "Default summary description")), Nil),
    shortName = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "Default short name")), Nil),
    summaryName = SmartString(localised = LocalisedString(m = Map(LangADT.En -> "Default summary name")), Nil),
    includeIf = None,
    pages = pages,
    repeatsUntil = None,
    repeatsWhile = None,
    repeaterContinueLabel = None,
    addAnotherQuestion = simpleFormComponent,
    instruction = None,
    presentationHint = None,
    infoMessage = None,
    errorMessage = None,
    defaultPage = None,
    cyaPage = None,
    fields = None,
    pageIdToDisplayAfterRemove = None
  )

  def mkTaskListNonRepeatingPages(
    page1: Page,
    page2: Page,
    taskCaption: Option[SmartString]
  ): (Section, Section, List[Section]) = {
    val section1 = Section.NonRepeatingPage(page = page1)
    val section2 = Section.NonRepeatingPage(page = page2)

    val expectedSections: List[Section] =
      List(
        page1.copy(caption = if (page1.caption.isEmpty) taskCaption else page1.caption),
        page2.copy(caption = if (page2.caption.isEmpty) taskCaption else page2.caption)
      )
        .map(Section.NonRepeatingPage)

    (section1, section2, expectedSections)
  }

  def mkTaskListRepeatingPages(
    page1: Page,
    page2: Page,
    taskCaption: Option[SmartString]
  ): (Section, Section, List[Section]) = {
    val section1 = Section.RepeatingPage(page = page1, Count(FormComponentId("Form component ID")))
    val section2 = Section.RepeatingPage(page = page2, Count(FormComponentId("Form component ID")))

    val expectedSections: List[Section] =
      List(
        page1.copy(caption = if (page1.caption.isEmpty) taskCaption else page1.caption),
        page2.copy(caption = if (page2.caption.isEmpty) taskCaption else page2.caption)
      )
        .map(Section.RepeatingPage(_, Count(FormComponentId("Form component ID"))))

    (section1, section2, expectedSections)
  }

  def mkTaskListAddToList(page1: Page, page2: Page): (Section, Section, List[Section]) = {
    val section1 = mkDefaultAtl(NonEmptyList(page1, List(page2)))
    val section2 = mkDefaultAtl(NonEmptyList(page2, List(page1)))

    val expectedSections: List[Section] =
      List(
        section1,
        section2
      )

    (section1, section2, expectedSections)
  }

  def mkTaskListNonRepeatingPageAndRepeatingPage(
    page1: Page,
    page2: Page,
    taskCaption: Option[SmartString]
  ): (Section, Section, List[Section]) = {
    val section1 = Section.NonRepeatingPage(page = page1)
    val section2 = Section.RepeatingPage(page = page2, Count(FormComponentId("Form component ID")))

    val expectedSections: List[Section] =
      List(
        Section.NonRepeatingPage(page =
          page1.copy(caption = if (page1.caption.isEmpty) taskCaption else page1.caption)
        ),
        Section.RepeatingPage(
          page = page2.copy(caption = if (page2.caption.isEmpty) taskCaption else page2.caption),
          repeats = Count(FormComponentId("Form component ID"))
        )
      )

    (section1, section2, expectedSections)
  }

  def mkTaskListNonRepeatingPageAndAddToList(
    page1: Page,
    page2: Page,
    taskCaption: Option[SmartString]
  ): (Section, Section, List[Section]) = {
    val section1 = Section.NonRepeatingPage(page = page1)
    val section2 = mkDefaultAtl(NonEmptyList(page2, List(page1)))

    val expectedSections: List[Section] =
      List(
        Section.NonRepeatingPage(page =
          page1.copy(caption = if (page1.caption.isEmpty) taskCaption else page1.caption)
        ),
        section2
      )

    (section1, section2, expectedSections)
  }

  def mkTaskListRepeatingPageAndAddToList(
    page1: Page,
    page2: Page,
    taskCaption: Option[SmartString]
  ): (Section, Section, List[Section]) = {
    val section1 = Section.RepeatingPage(page = page1, Count(FormComponentId("Form component ID")))
    val section2 = mkDefaultAtl(NonEmptyList(page2, List(page1)))

    val expectedSections: List[Section] =
      List(
        Section.RepeatingPage(
          page = page1.copy(caption = if (page1.caption.isEmpty) taskCaption else page1.caption),
          repeats = Count(FormComponentId("Form component ID"))
        ),
        section2
      )

    (section1, section2, expectedSections)
  }
}
