/*
 * Copyright 2022 HM Revenue & Customs
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

trait Substituter[A, T] {
  def substitute(substitutions: A, t: T): T
}

object Substituter {

  implicit class SubstituterSyntax[A, T](t: T)(implicit ev: Substituter[A, T]) {
    def apply(substitutions: A): T = Substituter[A, T].substitute(substitutions, t)
  }

  def apply[A, T](implicit substituter: Substituter[A, T]): Substituter[A, T] = substituter

  implicit def nonEmptyListSubstituter[A, T](implicit ev: Substituter[A, T]) =
    new Substituter[A, NonEmptyList[T]] {
      def substitute(substitutions: A, ts: NonEmptyList[T]): NonEmptyList[T] = {
        val substituter = implicitly[Substituter[A, T]]
        ts.map(t => substituter.substitute(substitutions, t))
      }
    }

  implicit def listSubstituter[A, T](implicit ev: Substituter[A, T]) = new Substituter[A, List[T]] {
    def substitute(substitutions: A, ts: List[T]): List[T] = {
      val substituter = implicitly[Substituter[A, T]]
      ts.map(t => substituter.substitute(substitutions, t))
    }
  }
  implicit def optionSubstituter[A, T](implicit ev: Substituter[A, T]) = new Substituter[A, Option[T]] {
    def substitute(substitutions: A, ts: Option[T]): Option[T] = {
      val substituter = implicitly[Substituter[A, T]]
      ts.map(t => substituter.substitute(substitutions, t))
    }
  }

  implicit def smartStringSubstituter[A](implicit ev: Substituter[A, Expr]): Substituter[A, SmartString] =
    (substitutions, t) =>
      t.copy(
        interpolations = t.interpolations(substitutions)
      )

  implicit def includeIfSubstituter[A](implicit
    ev: Substituter[A, BooleanExpr]
  ): Substituter[A, IncludeIf] = (substitutions, t) => t.copy(booleanExpr = t.booleanExpr(substitutions))

  implicit def validIfSubstituter[A](implicit
    ev: Substituter[A, BooleanExpr]
  ): Substituter[A, ValidIf] = (substitutions, t) => t.copy(booleanExpr = t.booleanExpr(substitutions))

  implicit def validatorSubstituter[A](implicit
    ev: Substituter[A, Expr]
  ): Substituter[A, Validator] = (substitutions, t) =>
    t match {
      case HmrcRosmRegistrationCheckValidator(errorMessage, regime, utr, postcode) =>
        HmrcRosmRegistrationCheckValidator(errorMessage(substitutions), regime, utr, postcode)
    }
  implicit def overseasAddressValueSubstituter[A](implicit
    ev: Substituter[A, Expr]
  ): Substituter[A, OverseasAddress.Value] = (substitutions, t) =>
    t.copy(
      t.line1(substitutions),
      t.line2(substitutions),
      t.line3(substitutions),
      t.city(substitutions),
      t.postcode(substitutions),
      t.country(substitutions)
    )

  implicit def revealingChoiceElementSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, RevealingChoiceElement] = (substitutions, t) =>
    t.copy(
      choice = t.choice(substitutions),
      revealingFields = t.revealingFields(substitutions),
      hint = t.hint(substitutions)
    )
  implicit def componentTypeSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, ComponentType] = (substitutions, t) =>
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

      case d @ Date(_, _, _) => d
      case CalendarDate      => CalendarDate
      case TaxPeriodDate     => TaxPeriodDate
      case a @ Address(_)    => a
      case OverseasAddress(mandatoryFields, optionalFields, value, countryLookup) =>
        OverseasAddress(mandatoryFields, optionalFields, value(substitutions), countryLookup)

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
      case f @ FileUpload(_) => f
      case t @ Time(_, _)    => t
      case PostcodeLookup    => PostcodeLookup
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
    ev: Substituter[A, Expr]
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
      instruction = t.instruction(substitutions)
    )

  implicit def pageSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, Page] = (substitutions, t) =>
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
        noPIITitle = addToList.noPIITitle(substitutions),
        description = addToList.description(substitutions),
        shortName = addToList.shortName(substitutions),
        summaryName = addToList.summaryName(substitutions),
        includeIf = addToList.includeIf(substitutions),
        limit = addToList.limit(substitutions),
        pages = addToList.pages(substitutions),
        addAnotherQuestion = addToList.addAnotherQuestion(substitutions),
        instruction = addToList.instruction(substitutions),
        infoMessage = addToList.infoMessage(substitutions),
        defaultPage = addToList.defaultPage(substitutions),
        cyaPage = addToList.cyaPage(substitutions)
      )
    }
  implicit def addToListLimitSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, AddToListLimit] = (substitutions, t) =>
    t.copy(
      repeatsMax = t.repeatsMax(substitutions),
      field = t.field(substitutions)
    )
  implicit def printSectionPageSubstituter[A](implicit
    ev: Substituter[A, Expr]
  ): Substituter[A, PrintSection.Page] = (substitutions, t) =>
    t.copy(
      title = t.title(substitutions),
      instructions = t.instructions(substitutions)
    )
  implicit def printSectionPdfSubstituter[A](implicit
    ev: Substituter[A, Expr]
  ): Substituter[A, PrintSection.Pdf] = (substitutions, t) =>
    t.copy(
      header = t.header(substitutions),
      footer = t.footer(substitutions)
    )
  implicit def printSectionPdfNotificationSubstituter[A](implicit
    ev: Substituter[A, Expr]
  ): Substituter[A, PrintSection.PdfNotification] =
    (substitutions, t) =>
      t.copy(
        header = t.header(substitutions),
        footer = t.footer(substitutions)
      )

  implicit def acknowledgementSectionPdfSubstituter[A](implicit
    ev: Substituter[A, Expr]
  ): Substituter[A, AcknowledgementSectionPdf] =
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
      instructionPdf = t.instructionPdf(substitutions)
    )
  implicit def destinationSubstituter[A](implicit
    ev: Substituter[A, Expr]
  ): Substituter[A, Destination] = (substitutions, t) =>
    t match {
      case d: Destination.HmrcDms                => d.copy(customerId = d.customerId(substitutions))
      case d: Destination.Composite              => d.copy(destinations = d.destinations(substitutions))
      case d: Destination.SubmissionConsolidator => d.copy(customerId = d.customerId(substitutions))
      case otherwise                             => otherwise
    }

  implicit def declarationSectionSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, DeclarationSection] = (substitutions, t) =>
    t.copy(
      title = t.title(substitutions),
      noPIITitle = t.noPIITitle(substitutions),
      description = t.description(substitutions),
      shortName = t.shortName(substitutions),
      continueLabel = t.continueLabel(substitutions),
      fields = t.fields(substitutions)
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
      title = t.title.apply(substitutions),
      header = t.header(substitutions),
      footer = t.footer(substitutions),
      continueLabel = t.continueLabel(substitutions),
      fields = t.fields(substitutions)
    )

  implicit def cyaPageSubstituter[A](implicit
    ev: Substituter[A, Expr]
  ): Substituter[A, CheckYourAnswersPage] = (substitutions, t) =>
    t.copy(
      title = t.title(substitutions),
      header = t.header(substitutions),
      footer = t.footer(substitutions),
      continueLabel = t.continueLabel(substitutions)
    )

  implicit def formTemplateSubstituter[A](implicit
    ev: Substituter[A, Expr],
    ev2: Substituter[A, BooleanExpr]
  ): Substituter[A, FormTemplate] = (substitutions, t) =>
    t.copy(
      destinations = t.destinations(substitutions),
      sections = t.sections(substitutions),
      summarySection = t.summarySection(substitutions)
    )
}
