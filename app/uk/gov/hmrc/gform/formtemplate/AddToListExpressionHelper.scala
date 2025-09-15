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
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object AddToListExpressionHelper {
  def updateRequiredExpressionsInAtl(formTemplate: FormTemplate): FormTemplate = {
    val updateFormKind = formTemplate.formKind.fold[FormKind] { classic =>
      val sections = classic.sections.map(s => updateSection(s))
      FormKind.Classic(sections)
    } { taskList =>
      taskList.copy(
        sections = taskList.sections.map { taskSection =>
          taskSection.copy(tasks = taskSection.tasks.map { task =>
            task.copy(sections = task.sections.map(s => updateSection(s)))
          })
        }
      )
    }
    formTemplate.copy(formKind = updateFormKind)
  }

  private def updateSection(section: Section): Section =
    section.fold[Section](nonRepeatingPage =>
      nonRepeatingPage.copy(page = updatePage(nonRepeatingPage.page, insideAtl = false))
    )(_ => section)(addToList =>
      addToList.copy(
        pages = addToList.pages.map(p => updatePage(p, insideAtl = true)),
        fields = updateOptionalFields(addToList.fields)
      )
    )

  private def updatePage(page: Page, insideAtl: Boolean): Page = {
    val fields = updateFields(page.fields, insideAtl)
    page.copy(fields = fields)
  }

  private def updateOptionalFields(
    maybeFields: Option[NonEmptyList[FormComponent]]
  ): Option[NonEmptyList[FormComponent]] =
    maybeFields match {
      case Some(nel) => NonEmptyList.fromList(updateFields(nel.toList, insideAtl = false))
      case None      => None
    }

  private def updateFields(
    fields: List[FormComponent],
    insideAtl: Boolean
  ): List[FormComponent] =
    fields.map { field =>
      val updatedComponentType: ComponentType = field.`type` match {
        case PostcodeLookup(chooseAddressLabel, confirmAddressLabel, enterAddressLabel) =>
          PostcodeLookup(
            updateOptionalSmartString(chooseAddressLabel, insideAtl),
            updateOptionalSmartString(confirmAddressLabel, insideAtl),
            updateOptionalSmartString(enterAddressLabel, insideAtl)
          )
        case c @ Choice(_, _, _, _, hints, optionHelpText, _, _, _, _, _) =>
          c.copy(
            hints = hints.map(nel => nel.map(ss => updateSmartString(ss, insideAtl))),
            optionHelpText = optionHelpText.map(nel => nel.map(ss => updateSmartString(ss, insideAtl)))
          )
        case r @ RevealingChoice(options, _) =>
          r.copy(options =
            options.map(o =>
              o.copy(
                revealingFields = updateFields(o.revealingFields, insideAtl),
                hint = updateOptionalSmartString(o.hint, insideAtl)
              )
            )
          )
        case g @ Group(fields, _, _, repeatLabel, repeatAddAnotherText) =>
          g.copy(
            fields = updateFields(fields, insideAtl = insideAtl),
            repeatLabel = updateOptionalSmartString(repeatLabel, insideAtl),
            repeatAddAnotherText = updateOptionalSmartString(repeatAddAnotherText, insideAtl)
          )
        case i @ InformationMessage(_, infoText, summaryValue) =>
          i.copy(
            infoText = updateSmartString(infoText, insideAtl),
            summaryValue = updateOptionalSmartString(summaryValue, insideAtl)
          )
        case m @ MultiFileUpload(_, _, hint, uploadAnotherLabel, continueText, _, _) =>
          m.copy(
            hint = updateOptionalSmartString(hint, insideAtl),
            uploadAnotherLabel = updateOptionalSmartString(uploadAnotherLabel, insideAtl),
            continueText = updateOptionalSmartString(continueText, insideAtl)
          )
        case m @ MiniSummaryList(rows, _, _) =>
          m.copy(rows = updateMslRows(rows, insideAtl))
        case t @ TableComp(header, rows, summaryValue, _, _, _, _) =>
          t.copy(
            header = header.map(h => h.copy(label = updateSmartString(h.label, insideAtl))),
            rows = rows.map(r =>
              r.copy(
                values = r.values.map(v => v.copy(value = updateSmartString(v.value, insideAtl))),
                includeIf = updateOptionalIncludeIf(r.includeIf, insideAtl)
              )
            ),
            summaryValue = updateSmartString(summaryValue, insideAtl)
          )
        case _ => field.`type`
      }
      field.copy(
        `type` = updatedComponentType,
        label = updateSmartString(field.label, insideAtl),
        helpText = updateOptionalSmartString(field.helpText, insideAtl),
        shortName = updateOptionalSmartString(field.shortName, insideAtl),
        errorMessage = updateOptionalSmartString(field.errorMessage, insideAtl),
        errorShortName = updateOptionalSmartString(field.errorShortName, insideAtl),
        errorShortNameStart = updateOptionalSmartString(field.errorShortNameStart, insideAtl),
        errorExample = updateOptionalSmartString(field.errorExample, insideAtl),
        includeIf = updateOptionalIncludeIf(field.includeIf, insideAtl)
      )
    }

  private def updateMslRows(
    mslRows: List[MiniSummaryRow],
    insideAtl: Boolean
  ): List[MiniSummaryRow] =
    mslRows.map {
      case r @ MiniSummaryRow.ValueRow(key, mslValue, includeIf, _, _) =>
        val updatedValue = mslValue match {
          case MiniSummaryListValue.AnyExpr(expr)    => MiniSummaryListValue.AnyExpr(loopExpr(expr, insideAtl))
          case v @ MiniSummaryListValue.Reference(_) => v
        }
        r.copy(
          key = updateOptionalSmartString(key, insideAtl),
          value = updatedValue,
          includeIf = updateOptionalIncludeIf(includeIf, insideAtl)
        )
      case r @ MiniSummaryRow.SmartStringRow(key, ss, includeIf, _, _) =>
        r.copy(
          key = updateOptionalSmartString(key, insideAtl),
          value = updateSmartString(ss, insideAtl),
          includeIf = updateOptionalIncludeIf(includeIf, insideAtl)
        )
      case h @ MiniSummaryRow.HeaderRow(ss) =>
        h.copy(header = updateSmartString(ss, insideAtl))
      case MiniSummaryRow.ATLRow(atlId, includeIf, rows) =>
        MiniSummaryRow.ATLRow(
          atlId,
          updateOptionalIncludeIf(includeIf, insideAtl),
          updateMslRows(rows, insideAtl = insideAtl)
        )
    }

  private def updateOptionalSmartString(
    maybeSmartString: Option[SmartString],
    insideAtl: Boolean
  ): Option[SmartString] =
    maybeSmartString match {
      case Some(ss) => Some(updateSmartString(ss, insideAtl))
      case None     => None
    }

  private def updateSmartString(smartString: SmartString, insideAtl: Boolean): SmartString =
    smartString.updateInterpolations(expr => loopExpr(expr, insideAtl))

  private def updateOptionalIncludeIf(maybeIncludeIf: Option[IncludeIf], insideAtl: Boolean): Option[IncludeIf] =
    maybeIncludeIf match {
      case Some(includeIf) => Some(updateIncludeIf(includeIf, insideAtl))
      case None            => None
    }

  private def updateIncludeIf(includeIf: IncludeIf, insideAtl: Boolean): IncludeIf = {
    def loop(booleanExpr: BooleanExpr): BooleanExpr =
      booleanExpr match {
        case Equals(left, right)      => Equals(loopExpr(left, insideAtl), loopExpr(right, insideAtl))
        case GreaterThan(left, right) => GreaterThan(loopExpr(left, insideAtl), loopExpr(right, insideAtl))
        case GreaterThanOrEquals(left, right) =>
          GreaterThanOrEquals(loopExpr(left, insideAtl), loopExpr(right, insideAtl))
        case LessThan(left, right)            => LessThan(loopExpr(left, insideAtl), loopExpr(right, insideAtl))
        case LessThanOrEquals(left, right)    => LessThanOrEquals(loopExpr(left, insideAtl), loopExpr(right, insideAtl))
        case Not(e)                           => loop(e)
        case Or(left, right)                  => Or(loop(left), loop(right))
        case And(left, right)                 => And(loop(left), loop(right))
        case Contains(multiValueField, value) => Contains(multiValueField, loopExpr(value, insideAtl))
        case In(value, dataSource)            => In(loopExpr(value, insideAtl), dataSource)
        case MatchRegex(expr, regex)          => MatchRegex(loopExpr(expr, insideAtl), regex)
        case _                                => booleanExpr
      }
    IncludeIf(loop(includeIf.booleanExpr))
  }

  private def loopExpr(expr: Expr, insideAtl: Boolean): Expr = {
    def loop(expr: Expr): Expr =
      expr match {
        case Add(field1, field2)          => Add(loop(field1), loop(field2))
        case Multiply(field1, field2)     => Multiply(loop(field1), loop(field2))
        case Subtraction(field1, field2)  => Subtraction(loop(field1), loop(field2))
        case Divide(field1, field2)       => Divide(loop(field1), loop(field2))
        case HideZeroDecimals(expr)       => HideZeroDecimals(loop(expr))
        case IfElse(cond, field1, field2) => IfElse(cond, loop(field1), loop(field2))
        case Else(field1, field2)         => Else(loop(field1), loop(field2))
        case Sum(field1)                  => Sum(loop(field1))
        case Typed(expr, tpe)             => Typed(loop(expr), tpe)
        //Expressions updated depending on inside/outside ATL follow
        case ChoicesAvailable(formComponentId, _) => ChoicesAvailable(formComponentId, insideAtl)

        //Other non-impacted exprs require no-op
        case _ => expr
      }
    loop(expr)
  }
}
