/*
 * Copyright 2025 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ And, FormComponent, FormKind, FormTemplate, IncludeIf, MiniSummaryList, MiniSummaryListValue, MiniSummaryRow, Page, PageId, Section, TaskId, TaskSection }

object MiniSummaryListHelper {
  def updateAllMslIncludeIfs(formTemplate: FormTemplate): FormTemplate = {
    val updateFormKind = formTemplate.formKind.fold[FormKind] { classic =>
      val sections = classic.sections.map(s => updateSection(s, formTemplate))
      FormKind.Classic(sections)
    } { taskList =>
      taskList.copy(
        sections = taskList.sections.map { taskSection =>
          taskSection.copy(tasks = taskSection.tasks.map { task =>
            task.copy(sections = task.sections.map(s => updateSection(s, formTemplate)))
          })
        }
      )
    }
    val updatedSummary =
      formTemplate.summarySection.copy(fields =
        getSummarySectionFields(formTemplate.summarySection.fields, formTemplate)
      )
    formTemplate.copy(formKind = updateFormKind, summarySection = updatedSummary)
  }

  private def getSummarySectionFields(
    fields: Option[NonEmptyList[FormComponent]],
    formTemplate: FormTemplate
  ): Option[NonEmptyList[FormComponent]] =
    fields.fold(fields)(fs => NonEmptyList.fromList(updateMsl(fs.toList, formTemplate)))

  private def updateSection(section: Section, formTemplate: FormTemplate): Section =
    section.fold[Section](nonRepeatingPage =>
      nonRepeatingPage.copy(page = updatePage(nonRepeatingPage.page, formTemplate))
    )(repeatingPage => repeatingPage.copy(page = updatePage(repeatingPage.page, formTemplate)))(addToList =>
      addToList.copy(pages = addToList.pages.map(p => updatePage(p, formTemplate)))
    )

  private def updatePage(page: Page, formTemplate: FormTemplate): Page = {
    val fields = updateMsl(page.fields, formTemplate)
    page.copy(fields = fields)
  }

  private def updateMsl(
    fields: List[FormComponent],
    formTemplate: FormTemplate
  ): List[FormComponent] =
    fields.map(f =>
      f.`type` match {
        case MiniSummaryList(rows, displayInSummary, keyDisplayWidth) =>
          f.copy(`type` = MiniSummaryList(updateMslRows(rows, formTemplate), displayInSummary, keyDisplayWidth))
        case _ => f
      }
    )

  private def updateMslRows(
    mslRows: List[MiniSummaryRow],
    formTemplate: FormTemplate
  ): List[MiniSummaryRow] =
    mslRows.map {
      case MiniSummaryRow.ValueRow(key, MiniSummaryListValue.AnyExpr(exp), includeIf, pageId, taskId) =>
        MiniSummaryRow.ValueRow(
          key,
          MiniSummaryListValue.AnyExpr(exp),
          createIncludeIf(includeIf, pageId, taskId, formTemplate),
          pageId,
          taskId
        )
      case MiniSummaryRow.ValueRow(key, r, includeIf, pageId, taskId) =>
        MiniSummaryRow.ValueRow(
          key,
          r,
          createIncludeIf(includeIf, pageId, taskId, formTemplate),
          pageId,
          taskId
        )
      case MiniSummaryRow.SmartStringRow(key, r, includeIf, pageId, taskId) =>
        MiniSummaryRow.SmartStringRow(
          key,
          r,
          createIncludeIf(includeIf, pageId, taskId, formTemplate),
          pageId,
          taskId
        )
      case header @ MiniSummaryRow.HeaderRow(_) =>
        header
      case MiniSummaryRow.ATLRow(atlId, includeIf, rows) =>
        MiniSummaryRow.ATLRow(atlId, includeIf, updateMslRows(rows, formTemplate))
    }

  private def createIncludeIf(
    rowIncludeIf: Option[IncludeIf],
    pageIdOpt: Option[PageId],
    taskIdOpt: Option[TaskId],
    formTemplate: FormTemplate
  ): Option[IncludeIf] = {
    def combineIncludeIf(a: IncludeIf, b: IncludeIf): IncludeIf =
      new IncludeIf(And(a.booleanExpr, b.booleanExpr))

    def handlePagesParentTask(pageIncludeIf: Option[IncludeIf], taskIncludeIf: Option[IncludeIf]): Option[IncludeIf] =
      (pageIncludeIf, taskIncludeIf) match {
        case (Some(pageIf), Some(taskIf)) => Option(combineIncludeIf(pageIf, taskIf))
        case (None, Some(taskIf))         => Option(taskIf)
        case (Some(pageIf), None)         => Option(pageIf)
        case _                            => None
      }

    def extractPageIncludeIf(pageId: PageId, section: Section, taskIncludeIf: Option[IncludeIf]): Option[IncludeIf] =
      section match {
        case Section.NonRepeatingPage(page) =>
          page.id.filter(_ == pageId).flatMap(_ => handlePagesParentTask(page.includeIf, taskIncludeIf))
        case Section.RepeatingPage(rPage, _) =>
          rPage.id.filter(_ == pageId).flatMap(_ => handlePagesParentTask(rPage.includeIf, taskIncludeIf))
        case Section.AddToList(_, _, _, _, _, _, _, _, pages, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
          pages.toList.collectFirst {
            case page if page.id.contains(pageId) => handlePagesParentTask(page.includeIf, taskIncludeIf)
          }.flatten
      }

    def findPageIncludeIf(pageId: PageId, formTemplate: FormTemplate): Option[IncludeIf] =
      formTemplate.formKind.fold(_ =>
        formTemplate.formKind.allSections
          .map(section => extractPageIncludeIf(pageId, section, None))
          .collectFirst { case Some(includeIf) => includeIf }
      ) { taskList =>
        taskList.sections
          .flatMap(_.tasks)
          .flatMap(task => task.sections.map(section => extractPageIncludeIf(pageId, section, task.includeIf)))
          .collectFirst { case Some(includeIf) => includeIf }
      }

    def extractTaskIncludeIf(taskId: TaskId, taskSection: TaskSection): List[Option[IncludeIf]] =
      taskSection.tasks.toList.map { task =>
        task.id match {
          case Some(id) if id == taskId => task.includeIf
          case _                        => None
        }
      }

    def findTaskIncludeIf(taskId: TaskId, formTemplate: FormTemplate): Option[IncludeIf] =
      formTemplate.formKind.fold(_ => None: Option[IncludeIf]) { taskList =>
        taskList.sections.toList
          .flatMap(extractTaskIncludeIf(taskId, _))
          .find(_.isDefined)
          .flatten
      }

    def combineWithRow(foundIncludeIf: Option[IncludeIf]): Option[IncludeIf] =
      (foundIncludeIf, rowIncludeIf) match {
        case (Some(found), Some(row)) => Some(combineIncludeIf(found, row))
        case (found @ Some(_), None)  => found
        case (None, row)              => row
      }

    (pageIdOpt, taskIdOpt) match {
      case (Some(pageId), None) =>
        combineWithRow(findPageIncludeIf(pageId, formTemplate))
      case (None, Some(taskId)) =>
        combineWithRow(findTaskIncludeIf(taskId, formTemplate))
      case _ =>
        rowIncludeIf
    }

  }

}
