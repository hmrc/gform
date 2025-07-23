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
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve.ParamExpr
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._

trait Substituter[A, T] {
  def substitute(substitutions: A, t: T, ft: Option[FormTemplate] = None): T
}

object Substituter {

  implicit class SubstituterSyntax[A, T](t: T)(implicit ev: Substituter[A, T]) {
    def apply(substitutions: A): T = Substituter[A, T].substitute(substitutions, t)
    def applyWithTemplate(substitutions: A, ft: Option[FormTemplate]): T =
      Substituter[A, T].substitute(substitutions, t, ft)
  }

  def apply[A, T](implicit substituter: Substituter[A, T]): Substituter[A, T] = substituter

  implicit def nonEmptyListSubstituter[A, T](implicit ev: Substituter[A, T]): Substituter[A, NonEmptyList[T]] =
    new Substituter[A, NonEmptyList[T]] {
      def substitute(substitutions: A, ts: NonEmptyList[T], ft: Option[FormTemplate] = None): NonEmptyList[T] = {
        val substituter = implicitly[Substituter[A, T]]
        ts.map(t => substituter.substitute(substitutions, t, ft))
      }
    }

  implicit def listSubstituter[A, T](implicit ev: Substituter[A, T]): Substituter[A, List[T]] =
    new Substituter[A, List[T]] {
      def substitute(substitutions: A, ts: List[T], ft: Option[FormTemplate] = None): List[T] = {
        val substituter = implicitly[Substituter[A, T]]
        ts.map(t => substituter.substitute(substitutions, t, ft))
      }
    }

  implicit def optionSubstituter[A, T](implicit ev: Substituter[A, T]): Substituter[A, Option[T]] =
    new Substituter[A, Option[T]] {
      def substitute(substitutions: A, ts: Option[T], ft: Option[FormTemplate] = None): Option[T] = {
        val substituter = implicitly[Substituter[A, T]]
        ts.map(t => substituter.substitute(substitutions, t, ft))
      }
    }

  private def updateSpecimenSmartString(smartString: SmartString): SmartString =
    smartString
      .updateInterpolations {
        case _: IfElse                       => Constant("[dynamic value]")
        case s: Else                         => Constant("[dynamic value]")
        case FormCtx(FormComponentId(value)) => Constant(s"[$value]")
        case otherwise                       => otherwise
      }

  implicit def smartStringSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, SmartString] = { (substitutions, t, _) =>
    import shapeless._
    val specimentTypable = Typeable[SpecimenExprSubstitutions]
    val substituter = implicitly[Substituter[A, Expr]]
    if (specimentTypable.cast(substitutions).isEmpty) {
      t.updateInterpolations(expr => substituter.substitute(substitutions, expr))
        .updateIncludeIfs(t => t(substitutions))
    } else {
      updateSpecimenSmartString(t)
    }
  }

  implicit def includeIfSubstituter[A](implicit
    ev: Substituter[A, BooleanExpr]
  ): Substituter[A, IncludeIf] = (substitutions, t, _) => t.copy(booleanExpr = t.booleanExpr(substitutions))

  implicit def validIfSubstituter[A](implicit
    ev: Substituter[A, BooleanExpr]
  ): Substituter[A, ValidIf] = (substitutions, t, _) => t.copy(booleanExpr = t.booleanExpr(substitutions))

  implicit def removeItemIfSubstituter[A](implicit
    ev: Substituter[A, BooleanExpr]
  ): Substituter[A, RemoveItemIf] = (substitutions, t, _) => t.copy(booleanExpr = t.booleanExpr(substitutions))

  implicit def confirmationSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, Confirmation] = (substitutions, t, _) =>
    t.copy(question = t.question(substitutions), redirects = t.redirects(substitutions))

  implicit def optionDataSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, OptionData] = (substitutions, t, _) =>
    t match {
      case o: OptionData.IndexBased => o.copy(label = o.label(substitutions), includeIf = o.includeIf(substitutions))
      case o @ OptionData.ValueBased(_, _, _, _, OptionDataValue.StringBased(_), _) =>
        o.copy(label = o.label(substitutions), includeIf = o.includeIf(substitutions))
      case o @ OptionData.ValueBased(_, _, _, _, OptionDataValue.ExprBased(expr), _) =>
        o.copy(
          label = o.label(substitutions),
          includeIf = o.includeIf(substitutions),
          value = OptionDataValue.ExprBased(expr(substitutions))
        )
    }

  implicit def revealingChoiceElementSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, RevealingChoiceElement] = (substitutions, t, _) =>
    t.copy(
      choice = t.choice(substitutions),
      revealingFields = t.revealingFields(substitutions),
      hint = t.hint(substitutions)
    )

  implicit def miniSummaryListRowSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, MiniSummaryRow] = {
    def createIncludeIf(
      rowIncludeIf: Option[IncludeIf],
      pageIdOpt: Option[PageId],
      taskIdOpt: Option[TaskId],
      formTemplateOpt: Option[FormTemplate]
    ): Option[IncludeIf] = {
      def combineIncludeIf(a: IncludeIf, b: IncludeIf): IncludeIf =
        new IncludeIf(And(a.booleanExpr, b.booleanExpr))

      def extractPageIncludeIf(pageId: PageId, section: Section): Option[IncludeIf] = section match {
        case Section.NonRepeatingPage(page) =>
          page.id.filter(_ == pageId).flatMap(_ => page.includeIf)
        case Section.RepeatingPage(rPage, _) =>
          rPage.id.filter(_ == pageId).flatMap(_ => rPage.includeIf)
        case Section.AddToList(_, _, _, _, _, _, _, _, pages, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
          pages.toList.collectFirst {
            case page if page.id.contains(pageId) => page.includeIf
          }.flatten
      }

      def findPageIncludeIf(pageId: PageId, formTemplate: FormTemplate): Option[IncludeIf] =
        formTemplate.formKind.allSections
          .flatMap(extractPageIncludeIf(pageId, _))
          .headOption

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

      formTemplateOpt match {
        case None => rowIncludeIf
        case Some(formTemplate) =>
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

    (substitutions, t, formTemplate) =>
      t match {
        case MiniSummaryRow.ValueRow(key, MiniSummaryListValue.AnyExpr(exp), includeIf, pageId, taskId) =>
          MiniSummaryRow.ValueRow(
            key(substitutions),
            MiniSummaryListValue.AnyExpr(exp(substitutions)),
            createIncludeIf(includeIf, pageId, taskId, formTemplate).map(ifExpr =>
              Substituter[A, IncludeIf].substitute(substitutions, ifExpr)
            ),
            pageId,
            taskId
          )
        case MiniSummaryRow.ValueRow(key, r, includeIf, pageId, taskId) =>
          MiniSummaryRow.ValueRow(
            key.map(_(substitutions)),
            r,
            createIncludeIf(includeIf, pageId, taskId, formTemplate).map(ifExpr =>
              Substituter[A, IncludeIf].substitute(substitutions, ifExpr)
            ),
            pageId,
            taskId
          )
        case MiniSummaryRow.SmartStringRow(key, r, includeIf, pageId, taskId) =>
          MiniSummaryRow.SmartStringRow(
            key.map(_(substitutions)),
            r(substitutions),
            createIncludeIf(includeIf, pageId, taskId, formTemplate).map(ifExpr =>
              Substituter[A, IncludeIf].substitute(substitutions, ifExpr)
            ),
            pageId,
            taskId
          )
        case MiniSummaryRow.HeaderRow(header) =>
          MiniSummaryRow.HeaderRow(header(substitutions))
        case MiniSummaryRow.ATLRow(atlId, includeIf, rows) =>
          MiniSummaryRow.ATLRow(atlId, includeIf(substitutions), rows(substitutions))
      }
  }

  implicit def tableValue[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, TableValue] = (substitutions, t, _) =>
    t match {
      case TableValue(value, cssClass, colspan, rowspan) => TableValue(value(substitutions), cssClass, colspan, rowspan)
    }

  implicit def tableRowSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, TableValueRow] = (substitutions, t, ft) =>
    t match {
      case TableValueRow(values, includeIf, dynamic) =>
        TableValueRow(values(substitutions), includeIf(substitutions), dynamic)
    }

  implicit def tableHeaderCellSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, TableHeaderCell] = (substitutions, t, _) => t.copy(label = t.label(substitutions))

  implicit def componentTypeSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, ComponentType] = (substitutions, t, ft) =>
    t match {
      case Text(constraint, value, displayWidth, toUpperCase, prefix, suffix, priority) =>
        Text(
          constraint,
          value(substitutions),
          displayWidth,
          toUpperCase,
          prefix(substitutions),
          suffix(substitutions),
          priority
        )
      case TextArea(constraint, value, displayWidth, rows, displayCharCount, dataThreshold) =>
        TextArea(
          constraint,
          value(substitutions),
          displayWidth,
          rows,
          displayCharCount,
          dataThreshold
        )

      case d @ Date(_, _, _)                              => d
      case CalendarDate                                   => CalendarDate
      case TaxPeriodDate                                  => TaxPeriodDate
      case a @ Address(_, _, _, Some(expr))               => a.copy(value = Some(expr(substitutions)))
      case a @ Address(_, _, _, _)                        => a
      case o @ OverseasAddress(_, _, _, Some(expr), _, _) => o.copy(value = Some(expr(substitutions)))
      case o @ OverseasAddress(_, _, _, _, _, _)          => o

      case Choice(
            tpe,
            options,
            orientation,
            selections,
            hints,
            optionHelpText,
            dividerPositon,
            dividerText,
            noneChoice,
            noneChoiceError,
            hideChoicesSelected
          ) =>
        Choice(
          tpe,
          options(substitutions),
          orientation,
          selections,
          hints(substitutions),
          optionHelpText(substitutions),
          dividerPositon,
          dividerText,
          noneChoice,
          noneChoiceError,
          hideChoicesSelected
        )

      case RevealingChoice(options, multiValue) =>
        RevealingChoice(
          options(substitutions),
          multiValue
        )

      case HmrcTaxPeriod(idType, idNumber, regimeType) =>
        HmrcTaxPeriod(idType, idNumber(substitutions), regimeType)

      case Group(fields, repeatsMax, repeatsMin, repeatLabel, repeatAddAnotherText) =>
        Group(
          fields(substitutions),
          repeatsMax,
          repeatsMin,
          repeatLabel(substitutions),
          repeatAddAnotherText(substitutions)
        )

      case InformationMessage(infoType, infoText, summaryValue) =>
        InformationMessage(infoType, infoText(substitutions), summaryValue(substitutions))
      case f @ FileUpload(_, _) => f
      case f @ MultiFileUpload(_, _, _, _, _, _, _) =>
        MultiFileUpload(
          fileSizeLimit = f.fileSizeLimit,
          allowedFileTypes = f.allowedFileTypes,
          hint = f.hint(substitutions),
          uploadAnotherLabel = f.uploadAnotherLabel(substitutions),
          continueText = f.continueText(substitutions),
          minFiles = f.minFiles(substitutions),
          maxFiles = f.maxFiles(substitutions)
        )
      case t @ Time(_, _) => t
      case p @ PostcodeLookup(_, _, _) =>
        PostcodeLookup(
          chooseAddressLabel = p.chooseAddressLabel(substitutions),
          confirmAddressLabel = p.confirmAddressLabel(substitutions),
          enterAddressLabel = p.enterAddressLabel(substitutions)
        )
      case MiniSummaryList(rows, displayInSummary, keyDisplayWidth) =>
        MiniSummaryList(rows.applyWithTemplate(substitutions, ft), displayInSummary, keyDisplayWidth)
      case t: TableComp =>
        t.copy(
          header = t.header(substitutions),
          rows = t.rows(substitutions),
          summaryValue = t.summaryValue(substitutions)
        )
      case b: Button => b.copy(reference = b.reference(substitutions), amountInPence = b.amountInPence(substitutions))
    }

  implicit def formComponentValidatorSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, FormComponentValidator] = (substitutions, t, _) =>
    t.copy(
      validIf = t.validIf(substitutions),
      errorMessage = t.errorMessage(substitutions)
    )

  implicit def instructionSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, Instruction] = (substitutions, t, _) => t.copy(name = t.name(substitutions))

  implicit def formComponentSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, FormComponent] = (substitutions, t, ft) =>
    t.copy(
      `type` = t.`type`.applyWithTemplate(substitutions, ft),
      label = t.label(substitutions),
      helpText = t.helpText(substitutions),
      shortName = t.shortName(substitutions),
      includeIf = t.includeIf(substitutions),
      validIf = t.validIf(substitutions),
      errorMessage = t.errorMessage(substitutions),
      validators = t.validators(substitutions),
      instruction = t.instruction(substitutions),
      errorShortName = t.errorShortName(substitutions),
      errorShortNameStart = t.errorShortNameStart(substitutions),
      errorExample = t.errorExample(substitutions)
    )

  implicit def continueIfSubstituter[A](implicit
    ev: Substituter[A, BooleanExpr]
  ): Substituter[A, ContinueIf] = (substitutions, t, _) =>
    t match {
      case ContinueIf.Continue => ContinueIf.Continue
      case ContinueIf.Stop     => ContinueIf.Stop
      case ContinueIf.Conditional(booleanExpression) =>
        ContinueIf.Conditional(booleanExpression(substitutions))
    }

  implicit def pageSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, Page] = (substitutions, t, ft) =>
    t.copy(
      title = t.title(substitutions),
      noPIITitle = t.noPIITitle(substitutions),
      description = t.description(substitutions),
      shortName = t.shortName(substitutions),
      caption = t.caption(substitutions),
      includeIf = t.includeIf(substitutions),
      continueIf = t.continueIf(substitutions),
      fields = t.fields.applyWithTemplate(substitutions, ft),
      continueLabel = t.continueLabel(substitutions),
      instruction = t.instruction(substitutions),
      redirects = t.redirects(substitutions),
      dataRetrieve = t.dataRetrieve(substitutions),
      removeItemIf = t.removeItemIf(substitutions),
      confirmation = t.confirmation(substitutions)
    )

  implicit def dataRetrieveSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, DataRetrieve] = (substitutions, dataRetrieve, _) =>
    dataRetrieve.copy(
      `if` = dataRetrieve.`if`.map(includeIf => includeIf(substitutions)),
      params = dataRetrieve.params.map(paramExpr =>
        ParamExpr(
          paramExpr.parameter,
          paramExpr.expr(substitutions)
        )
      )
    )

  implicit def sectionSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, Section] = (substitutions, t, ft) =>
    t.fold[Section] { nonRepeatingPage =>
      nonRepeatingPage.copy(page = nonRepeatingPage.page.applyWithTemplate(substitutions, ft))
    } { repeatingPage =>
      repeatingPage.copy(page = repeatingPage.page.applyWithTemplate(substitutions, ft))
    } { addToList =>
      addToList.copy(
        title = addToList.title(substitutions),
        caption = addToList.caption(substitutions),
        noPIITitle = addToList.noPIITitle(substitutions),
        description = addToList.description(substitutions),
        summaryDescription = addToList.summaryDescription(substitutions),
        shortName = addToList.shortName(substitutions),
        summaryName = addToList.summaryName(substitutions),
        includeIf = addToList.includeIf(substitutions),
        pages = addToList.pages.applyWithTemplate(substitutions, ft),
        repeatsUntil = addToList.repeatsUntil(substitutions),
        repeatsWhile = addToList.repeatsWhile(substitutions),
        repeaterContinueLabel = addToList.repeaterContinueLabel(substitutions),
        addAnotherQuestion = addToList.addAnotherQuestion(substitutions),
        instruction = addToList.instruction(substitutions),
        infoMessage = addToList.infoMessage(substitutions),
        defaultPage = addToList.defaultPage(substitutions),
        cyaPage = addToList.cyaPage.applyWithTemplate(substitutions, ft),
        fields = addToList.fields(substitutions),
        errorMessage = addToList.errorMessage(substitutions),
        descriptionTotal = addToList.descriptionTotal(substitutions),
        declarationSection = addToList.declarationSection(substitutions)
      )
    }

  implicit def taskSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, Task] = (substitutions, t, ft) =>
    t.copy(
      title = t.title(substitutions),
      sections = t.sections.applyWithTemplate(substitutions, ft),
      summarySection = t.summarySection.applyWithTemplate(substitutions, ft),
      declarationSection = t.declarationSection(substitutions),
      includeIf = t.includeIf(substitutions),
      startIf = t.startIf(substitutions),
      notRequiredIf = t.notRequiredIf(substitutions)
    )

  implicit def taskSectionSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, TaskSection] = (substitutions, t, ft) =>
    t.copy(
      title = t.title(substitutions),
      tasks = t.tasks.applyWithTemplate(substitutions, ft)
    )

  implicit def descriptionSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, AtlDescription] = (substitutions, t, _) =>
    t match {
      case s: AtlDescription.SmartStringBased => s.copy(value = s.value(substitutions))
      case k: AtlDescription.KeyValueBased =>
        k.copy(key = k.key(substitutions), value = k.value(substitutions))
    }

  implicit def descriptionTotalSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, AtlDescription.KeyValueBased] = (substitutions, t, ft) =>
    t.copy(key = t.key(substitutions), value = t.value(substitutions))

  implicit def formKindSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, FormKind] = (substitutions, t, ft) =>
    t.fold[FormKind] { taskList =>
      taskList.copy(sections = taskList.sections.applyWithTemplate(substitutions, ft))
    } { classic =>
      classic.copy(sections = classic.sections.applyWithTemplate(substitutions, ft))
    }

  implicit def printSectionPageSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, PrintSection.Page] = (substitutions, t, _) =>
    t.copy(
      title = t.title(substitutions),
      instructions = t.instructions(substitutions)
    )

  implicit def printSectionPdfSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, PrintSection.Pdf] = (substitutions, t, _) =>
    t.copy(
      header = t.header(substitutions),
      footer = t.footer(substitutions)
    )

  implicit def printSectionPdfNotificationSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, PrintSection.PdfNotification] =
    (substitutions, t, ft) =>
      t.copy(
        header = t.header(substitutions),
        footer = t.footer(substitutions)
      )

  implicit def pdfContextSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, PdfCtx] =
    (substitutions, t, ft) =>
      t.copy(
        header = t.header(substitutions),
        footer = t.footer(substitutions)
      )

  implicit def acknowledgementSectionSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, AcknowledgementSection] = (substitutions, t, _) =>
    t.copy(
      title = t.title(substitutions),
      description = t.description(substitutions),
      shortName = t.shortName(substitutions),
      fields = t.fields(substitutions),
      pdf = t.pdf(substitutions),
      instructionPdf = t.instructionPdf(substitutions),
      noPIITitle = t.noPIITitle(substitutions)
    )

  implicit def desIncludeIfValueSubstituter[A](implicit
    ev: Substituter[A, IncludeIf]
  ): Substituter[A, DestinationIncludeIf] = (substitutions, t, _) =>
    t match {
      case d @ DestinationIncludeIf.IncludeIfValue(includeIf) => d.copy(value = includeIf(substitutions))
      case otherwise                                          => otherwise
    }

  implicit def destinationSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, DestinationIncludeIf]
  ): Substituter[A, Destination] = (substitutions, t, _) =>
    t match {
      case d: Destination.HmrcDms =>
        d.copy(customerId = d.customerId(substitutions), includeIf = d.includeIf(substitutions))
      case d: Destination.DataStore =>
        d.copy(taxpayerId = d.taxpayerId(substitutions), includeIf = d.includeIf(substitutions))
      case d: Destination.InfoArchive =>
        d.copy(
          includeIf = d.includeIf(substitutions),
          paymentReference = d.paymentReference(substitutions),
          nino = d.nino(substitutions),
          utr = d.utr(substitutions),
          postalCode = d.postalCode(substitutions)
        )
      case d: Destination.Composite =>
        d.copy(destinations = d.destinations(substitutions), includeIf = d.includeIf(substitutions))
      case d: Destination.SubmissionConsolidator =>
        d.copy(customerId = d.customerId(substitutions), includeIf = d.includeIf(substitutions))
      case d: Destination.Email             => d.copy(includeIf = d.includeIf(substitutions))
      case d: Destination.HandlebarsHttpApi => d.copy(includeIf = d.includeIf(substitutions))
      case d: Destination.StateTransition   => d.copy(includeIf = d.includeIf(substitutions))
      case otherwise                        => otherwise
    }

  implicit def declarationSectionSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, DeclarationSection] = (substitutions, t, _) =>
    t.copy(
      title = t.title(substitutions),
      caption = t.caption(substitutions),
      noPIITitle = t.noPIITitle(substitutions),
      description = t.description(substitutions),
      shortName = t.shortName(substitutions),
      continueLabel = t.continueLabel(substitutions),
      fields = t.fields(substitutions),
      includeIf = t.includeIf(substitutions)
    )

  implicit def destinationsSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, Destinations] = (substitutions, t, _) =>
    t match {
      case Destinations.DestinationList(destinations, acknowledgementSection, declarationSection) =>
        Destinations.DestinationList(
          destinations(substitutions),
          acknowledgementSection(substitutions),
          declarationSection(substitutions)
        )

      case Destinations.DestinationPrint(page, pdf, pdfNotification) =>
        Destinations.DestinationPrint(
          page(substitutions),
          pdf(substitutions),
          pdfNotification(substitutions)
        )
    }

  implicit def summarySectionSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, SummarySection] = (substitutions, t, ft) =>
    t.copy(
      title = t.title(substitutions),
      caption = t.caption(substitutions),
      header = t.header(substitutions),
      footer = t.footer(substitutions),
      continueLabel = t.continueLabel(substitutions),
      fields = t.fields.applyWithTemplate(substitutions, ft),
      includeIf = t.includeIf(substitutions),
      pdf = t.pdf(substitutions)
    )

  implicit def cyaPageSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, CheckYourAnswersPage] = (substitutions, t, ft) =>
    t.copy(
      title = t.title(substitutions),
      caption = t.caption(substitutions),
      updateTitle = t.updateTitle(substitutions),
      noPIITitle = t.noPIITitle(substitutions),
      noPIIUpdateTitle = t.noPIIUpdateTitle(substitutions),
      header = t.header(substitutions),
      footer = t.footer(substitutions),
      continueLabel = t.continueLabel(substitutions),
      removeItemIf = t.removeItemIf(substitutions),
      fields = t.fields.applyWithTemplate(substitutions, ft)
    )

  implicit def emailParameterSubstituter[A](implicit
    ev: Substituter[A, Expr]
  ): Substituter[A, EmailParameter] = (substitutions, t, _) => t.copy(value = t.value(substitutions))

  implicit def formTemplateSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, FormTemplate] = (substitutions, t, ft) =>
    t.copy(
      destinations = t.destinations(substitutions),
      formKind = t.formKind.applyWithTemplate(substitutions, ft),
      summarySection = t.summarySection.applyWithTemplate(substitutions, ft),
      emailParameters = t.emailParameters(substitutions),
      dataRetrieve = t.dataRetrieve(substitutions),
      exitPages = t.exitPages(substitutions)
    )

  implicit def redirectSubstituter[A](implicit
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, RedirectCtx] = (substitutions, r, _) =>
    r.copy(
      `if` = r.`if`(substitutions)
    )

  implicit def confirmationRedirectSubstituter[A](implicit
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, ConfirmationRedirect] = (substitutions, r, _) =>
    r.copy(
      `if` = r.`if`(substitutions)
    )

  implicit def exitPageSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, ExitPage] = (substitutions, t, _) =>
    t.copy(
      `if` = t.`if`(substitutions),
      label = t.label(substitutions),
      exitMessage = t.exitMessage(substitutions)
    )
}
