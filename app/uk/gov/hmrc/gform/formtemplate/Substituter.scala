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
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._

trait Substituter[T] {
  def substitute(substitutions: Substitutions, t: T): T
}

object Substituter {

  implicit private class SubstituterSyntax[T: Substituter](t: T) {
    def apply(substitutions: Substitutions): T = Substituter[T].substitute(substitutions, t)
  }

  def apply[T](implicit substituter: Substituter[T]): Substituter[T] = substituter

  implicit def nonEmptyListSubstituter[T: Substituter] = new Substituter[NonEmptyList[T]] {
    def substitute(substitutions: Substitutions, ts: NonEmptyList[T]): NonEmptyList[T] = {
      val substituter = implicitly[Substituter[T]]
      ts.map(t => substituter.substitute(substitutions, t))
    }
  }

  implicit def listSubstituter[T: Substituter] = new Substituter[List[T]] {
    def substitute(substitutions: Substitutions, ts: List[T]): List[T] = {
      val substituter = implicitly[Substituter[T]]
      ts.map(t => substituter.substitute(substitutions, t))
    }
  }
  implicit def optionSubstituter[T: Substituter] = new Substituter[Option[T]] {
    def substitute(substitutions: Substitutions, ts: Option[T]): Option[T] = {
      val substituter = implicitly[Substituter[T]]
      ts.map(t => substituter.substitute(substitutions, t))
    }
  }

  implicit val exprSubstituter: Substituter[Expr] = new Substituter[Expr] {
    def substitute(substitutions: Substitutions, t: Expr): Expr = t match {
      case Else(l, r)        => Else(substitute(substitutions, l), substitute(substitutions, r))
      case Add(l, r)         => Add(substitute(substitutions, l), substitute(substitutions, r))
      case Multiply(l, r)    => Multiply(substitute(substitutions, l), substitute(substitutions, r))
      case Subtraction(l, r) => Subtraction(substitute(substitutions, l), substitute(substitutions, r))
      case Period(l, r)      => Period(substitute(substitutions, l), substitute(substitutions, r))
      case Sum(l)            => Sum(substitute(substitutions, l))
      case PeriodExt(p, pe)  => PeriodExt(substitute(substitutions, p), pe)
      case DateCtx(dateExpr) => DateCtx(dateExpr(substitutions))
      case i @ IfElse(cond, l, r) =>
        IfElse(cond(substitutions), substitute(substitutions, l), substitute(substitutions, r))
      case f @ FormCtx(formComponentId) =>
        // Replace FormComponentId with top level expression if one exists
        substitutions.expressions.getOrElse(ExpressionId(formComponentId.value), f)
      case AddressLens(_, _)            => t
      case AuthCtx(_)                   => t
      case Constant(_)                  => t
      case Count(_)                     => t
      case FormTemplateCtx(_)           => t
      case HmrcRosmRegistrationCheck(_) => t
      case LangCtx                      => t
      case LinkCtx(_)                   => t
      case ParamCtx(_)                  => t
      case PeriodValue(_)               => t
      case UserCtx(_)                   => t
      case Value                        => t
    }
  }

  implicit val smartStringSubstituter: Substituter[SmartString] = (substitutions, t) =>
    t.copy(
      interpolations = t.interpolations(substitutions)
    )

  implicit val dateExprSubstituter: Substituter[DateExpr] = (substitutions, t) =>
    t match {
      case d @ DateFormCtxVar(FormCtx(formComponentId)) =>
        substitutions.expressions.get(ExpressionId(formComponentId.value)) match {
          case Some(DateCtx(dateExpr)) => dateExpr
          case Some(ctx @ FormCtx(_))  => DateFormCtxVar(ctx)
          case here                    => d
        }
      case d @ DateValueExpr(_) => d
      case DateExprWithOffset(dExpr, offset) =>
        dExpr(substitutions) match {
          case DateExprWithOffset(expr, innerOffset) => DateExprWithOffset(expr, innerOffset + offset)
          case other                                 => DateExprWithOffset(other, offset)
        }
    }

  implicit val booleanExprSubstituter: Substituter[BooleanExpr] = new Substituter[BooleanExpr] {
    def substitute(substitutions: Substitutions, t: BooleanExpr): BooleanExpr = t match {
      case Equals(l, r)                     => Equals(l(substitutions), r(substitutions))
      case GreaterThan(l, r)                => GreaterThan(l(substitutions), r(substitutions))
      case GreaterThanOrEquals(l, r)        => GreaterThanOrEquals(l(substitutions), r(substitutions))
      case LessThan(l, r)                   => LessThan(l(substitutions), r(substitutions))
      case LessThanOrEquals(l, r)           => LessThanOrEquals(l(substitutions), r(substitutions))
      case Not(e)                           => Not(substitute(substitutions, e))
      case Or(left, right)                  => Or(substitute(substitutions, left), substitute(substitutions, right))
      case And(left, right)                 => And(substitute(substitutions, left), substitute(substitutions, right))
      case IsTrue                           => IsTrue
      case IsFalse                          => IsFalse
      case Contains(multiValueField, value) => Contains(multiValueField, value(substitutions))
      case In(value, dataSource)            => In(value(substitutions), dataSource)
      case m @ MatchRegex(formCtx, regex)   => m
      case DateBefore(l, r)                 => DateBefore(l(substitutions), r(substitutions))
      case DateAfter(l, r)                  => DateAfter(l(substitutions), r(substitutions))
      case f @ FormPhase(value)             => f
    }
  }

  implicit val includeIfSubstituter: Substituter[IncludeIf] = (substitutions, t) =>
    t.copy(booleanExpr = t.booleanExpr(substitutions))

  implicit val validIfSubstituter: Substituter[ValidIf] = (substitutions, t) =>
    t.copy(booleanExpr = t.booleanExpr(substitutions))

  implicit val validatorSubstituter: Substituter[Validator] = (substitutions, t) =>
    t match {
      case HmrcRosmRegistrationCheckValidator(errorMessage, regime, utr, postcode) =>
        HmrcRosmRegistrationCheckValidator(errorMessage(substitutions), regime, utr, postcode)
      case BankAccountModulusCheck(errorMessage, accountNumber, sortCode) =>
        BankAccountModulusCheck(errorMessage(substitutions), accountNumber, sortCode)
    }

  implicit val overseasAddressValueSubstituter: Substituter[OverseasAddress.Value] = (substitutions, t) =>
    t.copy(
      t.line1(substitutions),
      t.line2(substitutions),
      t.line3(substitutions),
      t.city(substitutions),
      t.postcode(substitutions),
      t.country(substitutions)
    )

  implicit val revealingChoiceElementSubstituter: Substituter[RevealingChoiceElement] = (substitutions, t) =>
    t.copy(
      choice = t.choice(substitutions),
      revealingFields = t.revealingFields(substitutions),
      hint = t.hint(substitutions)
    )

  implicit val componentTypeSubstituter: Substituter[ComponentType] = (substitutions, t) =>
    t match {
      case Text(constraint, value, displayWidth, toUpperCase, prefix, suffix) =>
        Text(
          constraint,
          value(substitutions),
          displayWidth,
          toUpperCase,
          prefix(substitutions),
          suffix(substitutions)
        )
      case TextArea(constraint, value, displayWidth, rows, displayCharCount) =>
        TextArea(
          constraint,
          value(substitutions),
          displayWidth,
          rows,
          displayCharCount
        )

      case UkSortCode(value) => UkSortCode(value(substitutions))

      case d @ Date(_, _, _) => d
      case CalendarDate      => CalendarDate
      case a @ Address(_)    => a
      case OverseasAddress(mandatoryFields, optionalFields, value) =>
        OverseasAddress(mandatoryFields, optionalFields, value(substitutions))

      case Choice(tpe, options, orientation, selections, hints, optionHelpText) =>
        Choice(
          tpe,
          options(substitutions),
          orientation,
          selections,
          hints(substitutions),
          optionHelpText(substitutions)
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

      case InformationMessage(infoType, infoText) =>
        InformationMessage(infoType, infoText(substitutions))
      case FileUpload()   => FileUpload()
      case t @ Time(_, _) => t
    }

  implicit val formComponentValidatorSubstituter: Substituter[FormComponentValidator] = (substitutions, t) =>
    t.copy(
      validIf = t.validIf(substitutions),
      errorMessage = t.errorMessage(substitutions)
    )

  implicit val instructionSubstituter: Substituter[Instruction] = (substitutions, t) =>
    t.copy(name = t.name(substitutions))

  implicit val formComponentSubstituter: Substituter[FormComponent] = (substitutions, t) =>
    t.copy(
      `type` = t.`type`(substitutions),
      label = t.label(substitutions),
      helpText = t.helpText(substitutions),
      shortName = t.shortName(substitutions),
      includeIf = t.includeIf(substitutions),
      validIf = t.validIf(substitutions),
      errorMessage = t.errorMessage(substitutions),
      validators = t.validators(substitutions),
      instruction = t.instruction(substitutions)
    )

  implicit val pageSubstituter: Substituter[Page] = (substitutions, t) =>
    t.copy(
      title = t.title(substitutions),
      noPIITitle = t.noPIITitle(substitutions),
      description = t.description(substitutions),
      shortName = t.shortName(substitutions),
      progressIndicator = t.progressIndicator(substitutions),
      includeIf = t.includeIf(substitutions),
      validators = t.validators(substitutions),
      fields = t.fields(substitutions),
      continueLabel = t.continueLabel(substitutions),
      instruction = t.instruction(substitutions)
    )

  implicit val sectionSubstituter: Substituter[Section] = (substitutions, t) =>
    t.fold[Section] { nonRepeatingPage =>
      nonRepeatingPage.copy(page = nonRepeatingPage.page(substitutions))
    } { repeatingPage =>
      repeatingPage.copy(page = repeatingPage.page(substitutions))
    } { addToList =>
      addToList.copy(
        title = addToList.title(substitutions),
        noPIITitle = addToList.noPIITitle(substitutions),
        description = addToList.description(substitutions),
        shortName = addToList.shortName(substitutions),
        summaryName = addToList.summaryName(substitutions),
        includeIf = addToList.includeIf(substitutions),
        repeatsMax = addToList.repeatsMax(substitutions),
        pages = addToList.pages(substitutions),
        addAnotherQuestion = addToList.addAnotherQuestion(substitutions),
        instruction = addToList.instruction(substitutions),
        infoMessage = addToList.infoMessage(substitutions),
        defaultPage = addToList.defaultPage(substitutions)
      )
    }

  implicit val printSectionPageSubstituter: Substituter[PrintSection.Page] = (substitutions, t) =>
    t.copy(
      title = t.title(substitutions),
      instructions = t.instructions(substitutions)
    )
  implicit val printSectionPdfSubstituter: Substituter[PrintSection.Pdf] = (substitutions, t) =>
    t.copy(
      header = t.header(substitutions),
      footer = t.footer(substitutions)
    )
  implicit val printSectionPdfNotificationSubstituter: Substituter[PrintSection.PdfNotification] = (substitutions, t) =>
    t.copy(
      header = t.header(substitutions),
      footer = t.footer(substitutions)
    )

  implicit val acknowledgementSectionPdfSubstituter: Substituter[AcknowledgementSectionPdf] = (substitutions, t) =>
    t.copy(
      header = t.header(substitutions),
      footer = t.footer(substitutions)
    )

  implicit val acknowledgementSectionSubstituter: Substituter[AcknowledgementSection] = (substitutions, t) =>
    t.copy(
      title = t.title(substitutions),
      description = t.description(substitutions),
      shortName = t.shortName(substitutions),
      fields = t.fields(substitutions),
      pdf = t.pdf(substitutions),
      instructionPdf = t.instructionPdf(substitutions)
    )

  implicit val destinationSubstituter: Substituter[Destination] = (substitutions, t) =>
    t match {
      case d: Destination.HmrcDms                => d.copy(customerId = d.customerId(substitutions))
      case d: Destination.Composite              => d.copy(destinations = d.destinations(substitutions))
      case d: Destination.SubmissionConsolidator => d.copy(customerId = d.customerId(substitutions))
      case otherwise                             => otherwise
    }

  implicit val declarationSectionSubstituter: Substituter[DeclarationSection] = (substitutions, t) =>
    t.copy(
      title = t.title(substitutions),
      noPIITitle = t.noPIITitle(substitutions),
      description = t.description(substitutions),
      shortName = t.shortName(substitutions),
      continueLabel = t.continueLabel(substitutions),
      fields = t.fields(substitutions)
    )

  implicit val destinationsSubstituter: Substituter[Destinations] = (substitutions, t) =>
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

  implicit val summarySectionSubstituter: Substituter[SummarySection] = (substitutions, t) =>
    t.copy(
      title = t.title(substitutions),
      header = t.header(substitutions),
      footer = t.footer(substitutions),
      continueLabel = t.continueLabel(substitutions)
    )

  implicit val formTemplateSubstituter: Substituter[FormTemplate] = (substitutions, t) =>
    t.copy(
      destinations = t.destinations(substitutions),
      sections = t.sections(substitutions),
      summarySection = t.summarySection(substitutions)
    )
}

trait SubstituteExpressions {

  def substituteExpressions(formTemplate: FormTemplate, substitutions: Substitutions): FormTemplate =
    Substituter[FormTemplate].substitute(substitutions, formTemplate)

}
