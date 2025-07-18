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
  def substitute(substitutions: A, t: T): T
}

object Substituter {

  implicit class SubstituterSyntax[A, T](t: T)(implicit ev: Substituter[A, T]) {
    def apply(substitutions: A): T = Substituter[A, T].substitute(substitutions, t)
  }

  def apply[A, T](implicit substituter: Substituter[A, T]): Substituter[A, T] = substituter

  implicit def nonEmptyListSubstituter[A, T](implicit ev: Substituter[A, T]): Substituter[A, NonEmptyList[T]] =
    new Substituter[A, NonEmptyList[T]] {
      def substitute(substitutions: A, ts: NonEmptyList[T]): NonEmptyList[T] = {
        val substituter = implicitly[Substituter[A, T]]
        ts.map(t => substituter.substitute(substitutions, t))
      }
    }

  implicit def listSubstituter[A, T](implicit ev: Substituter[A, T]): Substituter[A, List[T]] =
    new Substituter[A, List[T]] {
      def substitute(substitutions: A, ts: List[T]): List[T] = {
        val substituter = implicitly[Substituter[A, T]]
        ts.map(t => substituter.substitute(substitutions, t))
      }
    }

  implicit def optionSubstituter[A, T](implicit ev: Substituter[A, T]): Substituter[A, Option[T]] =
    new Substituter[A, Option[T]] {
      def substitute(substitutions: A, ts: Option[T]): Option[T] = {
        val substituter = implicitly[Substituter[A, T]]
        ts.map(t => substituter.substitute(substitutions, t))
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
  ): Substituter[A, SmartString] = { (substitutions, t) =>
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
  ): Substituter[A, IncludeIf] = (substitutions, t) => t.copy(booleanExpr = t.booleanExpr(substitutions))

  implicit def validIfSubstituter[A](implicit
    ev: Substituter[A, BooleanExpr]
  ): Substituter[A, ValidIf] = (substitutions, t) => t.copy(booleanExpr = t.booleanExpr(substitutions))

  implicit def removeItemIfSubstituter[A](implicit
    ev: Substituter[A, BooleanExpr]
  ): Substituter[A, RemoveItemIf] = (substitutions, t) => t.copy(booleanExpr = t.booleanExpr(substitutions))

  implicit def confirmationSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, Confirmation] = (substitutions, t) =>
    t.copy(question = t.question(substitutions), redirects = t.redirects(substitutions))

  implicit def optionDataSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, OptionData] = (substitutions, t) =>
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
  ): Substituter[A, RevealingChoiceElement] = (substitutions, t) =>
    t.copy(
      choice = t.choice(substitutions),
      revealingFields = t.revealingFields(substitutions),
      hint = t.hint(substitutions)
    )

  implicit def miniSummaryListRowSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, MiniSummaryRow] = (substitutions, t) =>
    t match {
      case MiniSummaryRow.ValueRow(key, MiniSummaryListValue.AnyExpr(exp), includeIf, pageId, taskId) =>
        MiniSummaryRow.ValueRow(
          key(substitutions),
          MiniSummaryListValue.AnyExpr(exp(substitutions)),
          includeIf(substitutions),
          pageId,
          taskId
        )
      case MiniSummaryRow.ValueRow(key, r, includeIf, pageId, taskId) =>
        MiniSummaryRow.ValueRow(key.map(_(substitutions)), r, includeIf(substitutions), pageId, taskId)
      case MiniSummaryRow.SmartStringRow(key, r, includeIf, pageId, taskId) =>
        MiniSummaryRow.SmartStringRow(
          key.map(_(substitutions)),
          r(substitutions),
          includeIf(substitutions),
          pageId,
          taskId
        )
      case MiniSummaryRow.HeaderRow(header) =>
        MiniSummaryRow.HeaderRow(header(substitutions))
      case MiniSummaryRow.ATLRow(atlId, includeIf, rows) =>
        MiniSummaryRow.ATLRow(atlId, includeIf(substitutions), rows(substitutions))
    }

  implicit def tableValue[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, TableValue] = (substitutions, t) =>
    t match {
      case TableValue(value, cssClass, colspan, rowspan) => TableValue(value(substitutions), cssClass, colspan, rowspan)
    }

  implicit def tableRowSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, TableValueRow] = (substitutions, t) =>
    t match {
      case TableValueRow(values, includeIf, dynamic) =>
        TableValueRow(values(substitutions), includeIf(substitutions), dynamic)
    }

  implicit def tableHeaderCellSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, TableHeaderCell] = (substitutions, t) => t.copy(label = t.label(substitutions))

  implicit def componentTypeSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, ComponentType] = (substitutions, t) =>
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
        MiniSummaryList(rows(substitutions), displayInSummary, keyDisplayWidth)
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
  ): Substituter[A, FormComponentValidator] = (substitutions, t) =>
    t.copy(
      validIf = t.validIf(substitutions),
      errorMessage = t.errorMessage(substitutions)
    )

  implicit def instructionSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, Instruction] = (substitutions, t) => t.copy(name = t.name(substitutions))

  implicit def formComponentSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, FormComponent] = (substitutions, t) =>
    t.copy(
      `type` = t.`type`(substitutions),
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
  ): Substituter[A, ContinueIf] = (substitutions, t) =>
    t match {
      case ContinueIf.Continue => ContinueIf.Continue
      case ContinueIf.Stop     => ContinueIf.Stop
      case ContinueIf.Conditional(booleanExpression) =>
        ContinueIf.Conditional(booleanExpression(substitutions))
    }

  implicit def pageSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, Page] = (substitutions, t) =>
    t.copy(
      title = t.title(substitutions),
      noPIITitle = t.noPIITitle(substitutions),
      description = t.description(substitutions),
      shortName = t.shortName(substitutions),
      caption = t.caption(substitutions),
      includeIf = t.includeIf(substitutions),
      continueIf = t.continueIf(substitutions),
      fields = t.fields(substitutions),
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
  ): Substituter[A, DataRetrieve] = (substitutions, dataRetrieve) =>
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
  ): Substituter[A, Section] = (substitutions, t) =>
    t.fold[Section] { nonRepeatingPage =>
      nonRepeatingPage.copy(page = nonRepeatingPage.page(substitutions))
    } { repeatingPage =>
      repeatingPage.copy(page = repeatingPage.page(substitutions))
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
        pages = addToList.pages(substitutions),
        repeatsUntil = addToList.repeatsUntil(substitutions),
        repeatsWhile = addToList.repeatsWhile(substitutions),
        repeaterContinueLabel = addToList.repeaterContinueLabel(substitutions),
        addAnotherQuestion = addToList.addAnotherQuestion(substitutions),
        instruction = addToList.instruction(substitutions),
        infoMessage = addToList.infoMessage(substitutions),
        defaultPage = addToList.defaultPage(substitutions),
        cyaPage = addToList.cyaPage(substitutions),
        fields = addToList.fields(substitutions),
        errorMessage = addToList.errorMessage(substitutions),
        descriptionTotal = addToList.descriptionTotal(substitutions),
        declarationSection = addToList.declarationSection(substitutions)
      )
    }

  implicit def taskSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, Task] = (substitutions, t) =>
    t.copy(
      title = t.title(substitutions),
      sections = t.sections(substitutions),
      summarySection = t.summarySection(substitutions),
      declarationSection = t.declarationSection(substitutions),
      includeIf = t.includeIf(substitutions),
      startIf = t.startIf(substitutions),
      notRequiredIf = t.notRequiredIf(substitutions)
    )

  implicit def taskSectionSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, TaskSection] = (substitutions, t) =>
    t.copy(
      title = t.title(substitutions),
      tasks = t.tasks(substitutions)
    )

  implicit def descriptionSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, AtlDescription] = (substitutions, t) =>
    t match {
      case s: AtlDescription.SmartStringBased => s.copy(value = s.value(substitutions))
      case k: AtlDescription.KeyValueBased =>
        k.copy(key = k.key(substitutions), value = k.value(substitutions))
    }

  implicit def descriptionTotalSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, AtlDescription.KeyValueBased] = (substitutions, t) =>
    t.copy(key = t.key(substitutions), value = t.value(substitutions))

  implicit def formKindSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, FormKind] = (substitutions, t) =>
    t.fold[FormKind] { taskList =>
      taskList.copy(sections = taskList.sections(substitutions))
    } { classic =>
      classic.copy(sections = classic.sections(substitutions))
    }

  implicit def printSectionPageSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, PrintSection.Page] = (substitutions, t) =>
    t.copy(
      title = t.title(substitutions),
      instructions = t.instructions(substitutions)
    )

  implicit def printSectionPdfSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, PrintSection.Pdf] = (substitutions, t) =>
    t.copy(
      header = t.header(substitutions),
      footer = t.footer(substitutions)
    )

  implicit def printSectionPdfNotificationSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, PrintSection.PdfNotification] =
    (substitutions, t) =>
      t.copy(
        header = t.header(substitutions),
        footer = t.footer(substitutions)
      )

  implicit def pdfContextSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, PdfCtx] =
    (substitutions, t) =>
      t.copy(
        header = t.header(substitutions),
        footer = t.footer(substitutions)
      )

  implicit def acknowledgementSectionSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, AcknowledgementSection] = (substitutions, t) =>
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
  ): Substituter[A, DestinationIncludeIf] = (substitutions, t) =>
    t match {
      case d @ DestinationIncludeIf.IncludeIfValue(includeIf) => d.copy(value = includeIf(substitutions))
      case otherwise                                          => otherwise
    }

  implicit def destinationSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, DestinationIncludeIf]
  ): Substituter[A, Destination] = (substitutions, t) =>
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
  ): Substituter[A, DeclarationSection] = (substitutions, t) =>
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
  ): Substituter[A, Destinations] = (substitutions, t) =>
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
  ): Substituter[A, SummarySection] = (substitutions, t) =>
    t.copy(
      title = t.title(substitutions),
      caption = t.caption(substitutions),
      header = t.header(substitutions),
      footer = t.footer(substitutions),
      continueLabel = t.continueLabel(substitutions),
      fields = t.fields(substitutions),
      includeIf = t.includeIf(substitutions),
      pdf = t.pdf(substitutions)
    )

  implicit def cyaPageSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, CheckYourAnswersPage] = (substitutions, t) =>
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
      fields = t.fields(substitutions)
    )

  implicit def emailParameterSubstituter[A](implicit
    ev: Substituter[A, Expr]
  ): Substituter[A, EmailParameter] = (substitutions, t) => t.copy(value = t.value(substitutions))

  implicit def formTemplateSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, FormTemplate] = (substitutions, t) =>
    t.copy(
      destinations = t.destinations(substitutions),
      formKind = t.formKind(substitutions),
      summarySection = t.summarySection(substitutions),
      emailParameters = t.emailParameters(substitutions),
      dataRetrieve = t.dataRetrieve(substitutions),
      exitPages = t.exitPages(substitutions)
    )

  implicit def redirectSubstituter[A](implicit
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, RedirectCtx] = (substitutions, r) =>
    r.copy(
      `if` = r.`if`(substitutions)
    )

  implicit def confirmationRedirectSubstituter[A](implicit
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, ConfirmationRedirect] = (substitutions, r) =>
    r.copy(
      `if` = r.`if`(substitutions)
    )

  implicit def exitPageSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, ExitPage] = (substitutions, t) =>
    t.copy(
      `if` = t.`if`(substitutions),
      label = t.label(substitutions),
      exitMessage = t.exitMessage(substitutions)
    )
}
