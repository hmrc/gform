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

  private case class UpdateContext(insideAtl: Boolean)

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
    section.fold[Section] { nonRepeatingPage =>
      implicit val ctx: UpdateContext = UpdateContext(insideAtl = false)
      nonRepeatingPage.copy(page = updatePage(nonRepeatingPage.page))
    }(_ => section) { addToList =>
      implicit val ctx: UpdateContext = UpdateContext(insideAtl = true)
      addToList.copy(
        pages = addToList.pages.map(p => updatePage(p)),
        fields = updateOptionalFields(addToList.fields)
      )
    }

  private def updatePage(page: Page)(implicit ctx: UpdateContext): Page = {
    val fields = updateFields(page.fields)
    page.copy(fields = fields)
  }

  private def updateOptionalFields(
    maybeFields: Option[NonEmptyList[FormComponent]]
  ): Option[NonEmptyList[FormComponent]] = {
    implicit val nonAtlCtx: UpdateContext = UpdateContext(insideAtl = false)
    maybeFields match {
      case Some(nel) => NonEmptyList.fromList(updateFields(nel.toList)(nonAtlCtx))
      case None      => None
    }
  }

  private def updateFields(
    fields: List[FormComponent]
  )(implicit ctx: UpdateContext): List[FormComponent] =
    fields.map { field =>
      val updatedComponentType: ComponentType = field.`type` match {
        case PostcodeLookup(chooseAddressLabel, confirmAddressLabel, enterAddressLabel) =>
          PostcodeLookup(
            updateOptionalSmartString(chooseAddressLabel),
            updateOptionalSmartString(confirmAddressLabel),
            updateOptionalSmartString(enterAddressLabel)
          )
        case c @ Choice(_, _, _, _, hints, optionHelpText, _, _, _, _, _) =>
          c.copy(
            hints = hints.map(nel => nel.map(ss => updateSmartString(ss))),
            optionHelpText = optionHelpText.map(nel => nel.map(ss => updateSmartString(ss)))
          )
        case r @ RevealingChoice(options, _) =>
          r.copy(options =
            options.map(o =>
              o.copy(
                revealingFields = updateFields(o.revealingFields),
                hint = updateOptionalSmartString(o.hint)
              )
            )
          )
        case g @ Group(fields, _, _, repeatLabel, repeatAddAnotherText) =>
          g.copy(
            fields = updateFields(fields),
            repeatLabel = updateOptionalSmartString(repeatLabel),
            repeatAddAnotherText = updateOptionalSmartString(repeatAddAnotherText)
          )
        case i @ InformationMessage(_, infoText, summaryValue) =>
          i.copy(
            infoText = updateSmartString(infoText),
            summaryValue = updateOptionalSmartString(summaryValue)
          )
        case m @ MultiFileUpload(_, _, hint, uploadAnotherLabel, continueText, _, _) =>
          m.copy(
            hint = updateOptionalSmartString(hint),
            uploadAnotherLabel = updateOptionalSmartString(uploadAnotherLabel),
            continueText = updateOptionalSmartString(continueText)
          )
        case m @ MiniSummaryList(rows, _, _) =>
          m.copy(rows = updateMslRows(rows))
        case t @ TableComp(header, rows, summaryValue, _, _, _, _) =>
          t.copy(
            header = header.map(h => h.copy(label = updateSmartString(h.label))),
            rows = rows.map(r =>
              r.copy(
                values = r.values.map(v => v.copy(value = updateSmartString(v.value))),
                includeIf = updateOptionalIncludeIf(r.includeIf)
              )
            ),
            summaryValue = updateSmartString(summaryValue)
          )
        case _ => field.`type`
      }
      field.copy(
        `type` = updatedComponentType,
        label = updateSmartString(field.label),
        helpText = updateOptionalSmartString(field.helpText),
        shortName = updateOptionalSmartString(field.shortName),
        errorMessage = updateOptionalSmartString(field.errorMessage),
        errorShortName = updateOptionalSmartString(field.errorShortName),
        errorShortNameStart = updateOptionalSmartString(field.errorShortNameStart),
        errorExample = updateOptionalSmartString(field.errorExample),
        includeIf = updateOptionalIncludeIf(field.includeIf)
      )
    }

  private def updateMslRows(
    mslRows: List[MiniSummaryRow]
  )(implicit ctx: UpdateContext): List[MiniSummaryRow] =
    mslRows.map {
      case r @ MiniSummaryRow.ValueRow(key, mslValue, includeIf, _, _) =>
        val updatedValue = mslValue match {
          case MiniSummaryListValue.AnyExpr(expr)    => MiniSummaryListValue.AnyExpr(loopExpr(expr))
          case v @ MiniSummaryListValue.Reference(_) => v
        }
        r.copy(
          key = updateOptionalSmartString(key),
          value = updatedValue,
          includeIf = updateOptionalIncludeIf(includeIf)
        )
      case r @ MiniSummaryRow.SmartStringRow(key, ss, includeIf, _, _) =>
        r.copy(
          key = updateOptionalSmartString(key),
          value = updateSmartString(ss),
          includeIf = updateOptionalIncludeIf(includeIf)
        )
      case h @ MiniSummaryRow.HeaderRow(ss) =>
        h.copy(header = updateSmartString(ss))
      case MiniSummaryRow.ATLRow(atlId, includeIf, rows) =>
        MiniSummaryRow.ATLRow(
          atlId,
          updateOptionalIncludeIf(includeIf),
          updateMslRows(rows)
        )
    }

  private def updateOptionalSmartString(
    maybeSmartString: Option[SmartString]
  )(implicit ctx: UpdateContext): Option[SmartString] =
    maybeSmartString match {
      case Some(ss) => Some(updateSmartString(ss))
      case None     => None
    }

  private def updateSmartString(smartString: SmartString)(implicit ctx: UpdateContext): SmartString =
    smartString.updateInterpolations(expr => loopExpr(expr))

  private def updateOptionalIncludeIf(
    maybeIncludeIf: Option[IncludeIf]
  )(implicit ctx: UpdateContext): Option[IncludeIf] =
    maybeIncludeIf match {
      case Some(includeIf) => Some(updateIncludeIf(includeIf))
      case None            => None
    }

  private def updateIncludeIf(includeIf: IncludeIf)(implicit ctx: UpdateContext): IncludeIf = {
    def loop(booleanExpr: BooleanExpr): BooleanExpr =
      booleanExpr match {
        case Equals(left, right)      => Equals(loopExpr(left), loopExpr(right))
        case GreaterThan(left, right) => GreaterThan(loopExpr(left), loopExpr(right))
        case GreaterThanOrEquals(left, right) =>
          GreaterThanOrEquals(loopExpr(left), loopExpr(right))
        case LessThan(left, right)            => LessThan(loopExpr(left), loopExpr(right))
        case LessThanOrEquals(left, right)    => LessThanOrEquals(loopExpr(left), loopExpr(right))
        case Not(e)                           => Not(loop(e))
        case Or(left, right)                  => Or(loop(left), loop(right))
        case And(left, right)                 => And(loop(left), loop(right))
        case Contains(multiValueField, value) => Contains(multiValueField, loopExpr(value))
        case In(value, dataSource)            => In(loopExpr(value), dataSource)
        case MatchRegex(expr, regex)          => MatchRegex(loopExpr(expr), regex)
        case _                                => booleanExpr
      }
    IncludeIf(loop(includeIf.booleanExpr))
  }

  private def loopExpr(expr: Expr)(implicit ctx: UpdateContext): Expr = {
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
        //Expressions updated depending on inside/outside ATL - Start
        case ChoicesAvailable(formComponentId, _) => ChoicesAvailable(formComponentId, Some(ctx.insideAtl))
        case NumberedListChoicesSelected(formComponentId, _) =>
          NumberedListChoicesSelected(formComponentId, Some(ctx.insideAtl))
        case BulletedListChoicesSelected(formComponentId, _) =>
          BulletedListChoicesSelected(formComponentId, Some(ctx.insideAtl))
        //Expressions updated depending on inside/outside ATL - End
        case _ => expr
      }
    loop(expr)
  }
}
