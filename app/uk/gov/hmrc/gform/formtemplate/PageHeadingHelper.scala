/*
 * Copyright 2024 HM Revenue & Customs
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

import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormKind, FormTemplate, IsGroup, IsInformationMessage, Page, Section }

object PageHeadingHelper {
  def fillBlankPageHeadings(formTemplate: FormTemplate): FormTemplate = {
    val updateFormKind = formTemplate.formKind.fold[FormKind] { classic =>
      val sections = classic.sections.map(fillSection)
      FormKind.Classic(sections)
    } { taskList =>
      taskList.copy(
        sections = taskList.sections.map { taskSection =>
          taskSection.copy(tasks = taskSection.tasks.map { task =>
            task.copy(sections = task.sections.map(fillSection))
          })
        }
      )
    }
    formTemplate.copy(formKind = updateFormKind)
  }

  private def fillSection(section: Section): Section =
    section.fold[Section](nonRepeatingPage => nonRepeatingPage.copy(page = fillPage(nonRepeatingPage.page)))(
      repeatingPage => repeatingPage.copy(page = fillPage(repeatingPage.page))
    )(addToList => addToList.copy(pages = addToList.pages.map(fillPage)))

  private def wasMarkedAsEmpty(smartString: SmartString): Boolean = smartString.fold { base =>
    val internal = base.internal
    internal.localised.m.isEmpty && internal.interpolations.isEmpty
  }(cond => false)

  private def fillPage(page: Page): Page =
    page.fields match {
      case Nil => page
      case formComponent :: tail =>
        val tailIsNonEditable = tail.forall(formComponent =>
          formComponent match {
            case IsInformationMessage(_) => true
            case fc                      => fc.onlyShowOnSummary || !fc.editable
          }
        )
        formComponent match {
          case IsGroup(g)              => page
          case IsInformationMessage(_) => page
          case fc =>
            if (
              formComponent.editable &&
              formComponent.isPageHeading &&
              wasMarkedAsEmpty(formComponent.label) &&
              tailIsNonEditable
            )
              page.copy(fields = formComponent.copy(label = page.title) :: tail)
            else page
        }
    }
}
