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

import java.time.LocalDate

import cats.Monoid
import cats.implicits._
import scalax.collection.Graph
import scalax.collection.GraphEdge._
import uk.gov.hmrc.gform.core.{ Invalid, Valid, ValidationResult }
import uk.gov.hmrc.gform.core.ValidationResult.{ BooleanToValidationResultSyntax, validationResultMonoid }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.graph.DependencyGraph._
import uk.gov.hmrc.gform.formtemplate.FormTemplateValidatorHelper._
import uk.gov.hmrc.gform.sharedmodel.{ AvailableLanguages, LangADT }

import scala.Function.const
import scala.util.{ Failure, Success, Try }

object FormTemplateValidator {

  def validateLanguages(languageList: AvailableLanguages): ValidationResult =
    if (languageList.languages.contains(LangADT.En)) Valid
    else Invalid("languages must contain en")

  def fieldIds(sections: List[Section]): List[FormComponentId] =
    indexedFieldIds(sections).map(_._1) ::: SectionHelper.addToListRepeaters(sections).map(_.id)

  private def indexedFieldIds(sections: List[Section]): List[(FormComponentId, Int)] = indexedFields(sections).map {
    case (a, b) => a.id -> b
  }

  private def indexedFields(sections: List[Section]): List[(FormComponent, Int)] =
    SectionHelper.pages(sections).zipWithIndex.flatMap {
      case (section, idx) =>
        val standardFields = section.fields.map(_ -> idx)
        val subFields = section.fields
          .map(_.`type`)
          .collect {
            case Group(fields, _, _, _, _)   => fields.map(_ -> idx)
            case RevealingChoice(options, _) => options.toList.flatMap(_.revealingFields.map(_ -> idx))
          }

        standardFields ::: subFields.flatten
    }

  def someFieldsAreDefinedMoreThanOnce(duplicates: Set[FormComponentId]) =
    s"Some FieldIds are defined more than once: ${duplicates.toList.sortBy(_.value).map(_.value)}"

  def validateUniqueFields(sectionsList: List[Section]): ValidationResult = {
    val ids: List[FormComponentId] = fieldIds(sectionsList)

    val duplicates = ids.groupBy(identity).collect { case (fId, List(_, _, _*)) => fId }.toSet
    duplicates.isEmpty.validationResult(someFieldsAreDefinedMoreThanOnce(duplicates))
  }

  def validateInstructions(pages: List[Page]): ValidationResult =
    if (!pages.flatMap(_.instruction).flatMap(_.name).forall(_.nonEmpty)) {
      Invalid("One or more sections have instruction attribute with empty names")
    } else if (!pages.flatMap(_.instruction.flatMap(_.order)).forall(_ >= 0)) {
      Invalid("One or more sections have instruction attribute with negative order")
    } else if (pages.flatMap(_.instruction.flatMap(_.order)).distinct.size != pages
                 .flatMap(_.instruction.flatMap(_.order))
                 .size) {
      Invalid("One or more sections have instruction attribute with duplicate order value")
    } else if (!pages.flatMap(_.fields).flatMap(_.instruction).flatMap(_.name).forall(_.nonEmpty)) {
      Invalid("One or more section fields have instruction attribute with empty names")
    } else if (!pages.flatMap(_.fields).flatMap(_.instruction.flatMap(_.order)).forall(_ >= 0)) {
      Invalid("One or more section fields have instruction attribute with negative order")
    } else if (pages.forall(
                 p =>
                   p.fields.flatMap(_.instruction.flatMap(_.order)).distinct.size != p.fields
                     .flatMap(_.instruction.flatMap(_.order))
                     .size)) {
      Invalid("All fields within sections that have instruction defined, must have a unique order value")
    } else {
      Valid
    }

  def validateChoiceHelpText(sectionsList: List[Page]): ValidationResult = {
    val choiceFieldIds: List[FormComponentId] = sectionsList
      .flatMap(_.fields)
      .map(fv => (fv.id, fv.`type`))
      .collect {
        case (fId, Choice(_, options, _, _, helpTextList))
            if helpTextList.fold(false)(helpTexts => options.size != helpTexts.size) =>
          fId
      }

    choiceFieldIds.isEmpty.validationResult(
      s"Choice components doesn't have equal number of choices and help texts ${choiceFieldIds.mkString(",")}")
  }

  val userContextComponentType: List[FormComponent] => List[FormComponent] =
    enrolledIdentifierComponents =>
      enrolledIdentifierComponents.collect {
        case expr @ HasExpr(SingleExpr(UserCtx(UserField.EnrolledIdentifier))) => expr
    }

  def validateEnrolmentIdentifier(formTemplate: FormTemplate): ValidationResult =
    if (userContextComponentType(formTemplate.expandedFormComponentsInMainSections).nonEmpty) {
      formTemplate.authConfig match {
        case HmrcEnrolmentModule(_) | HmrcAgentWithEnrolmentModule(_, _) => Valid
        case _                                                           => Invalid("You used '${user.enrolledIdentifier}' but you didn't provide 'serviceId'.")
      }
    } else Valid

  def validateEnrolmentSection(formTemplate: FormTemplate): ValidationResult =
    formTemplate.authConfig match {
      case HasEnrolmentSection(_, enrolmentSection, _) =>
        val fcIds = enrolmentSection.fields.map(_.id).toSet
        val ctxs = enrolmentSection.identifiers.map(_.value.formComponentId).toList.toSet ++ enrolmentSection.verifiers
          .map(_.value.formComponentId)
          .toSet
        ctxs
          .subsetOf(fcIds)
          .validationResult(
            s"Following identifiers and/or verifiers don't have corresponding field entry: " + ctxs
              .diff(fcIds)
              .mkString(", "))
      case _ => Valid
    }

  def validateRegimeId(formTemplate: FormTemplate): ValidationResult = {
    def regimeIdCheck(regimeId: RegimeId): ValidationResult =
      (regimeId.value.length >= 2 && regimeId.value.length <= 8)
        .validationResult("Regime id must be between 2 and 8 characters long")

    formTemplate.authConfig match {
      case HmrcEnrolmentModule(EnrolmentAuth(_, DoCheck(_, _, RegimeIdCheck(regimeId)))) => regimeIdCheck(regimeId)
      case HmrcAgentWithEnrolmentModule(_, EnrolmentAuth(_, DoCheck(_, _, RegimeIdCheck(regimeId)))) =>
        regimeIdCheck(regimeId)
      case _ => Valid
    }
  }

  def validateDependencyGraph(formTemplate: FormTemplate): ValidationResult = {
    val graph: Graph[FormComponentId, DiEdge] = toGraph(formTemplate)
    constructDependencyGraph(graph)
      .bimap(const(Invalid(s"Graph contains cycle ${graph.findCycle}")), const(Valid))
      .merge
  }

  def validate(componentTypes: List[ComponentType], formTemplate: FormTemplate): ValidationResult = {
    val results = componentTypes.map(validate(_, formTemplate))
    Monoid[ValidationResult].combineAll(results)
  }

  def validate(componentType: ComponentType, formTemplate: FormTemplate): ValidationResult = componentType match {
    case HasExpr(SingleExpr(expr))     => validate(expr, formTemplate.sections)
    case HasExpr(MultipleExpr(fields)) => Valid
    case Date(_, _, _)                 => Valid
    case Address(_)                    => Valid
    case Choice(_, _, _, _, _)         => Valid
    case RevealingChoice(revealingChoiceElements, _) =>
      validate(revealingChoiceElements.toList.flatMap(_.revealingFields.map(_.`type`)), formTemplate)
    case HmrcTaxPeriod(_, _, _)   => Valid
    case Group(fvs, _, _, _, _)   => validate(fvs.map(_.`type`), formTemplate)
    case FileUpload()             => Valid
    case InformationMessage(_, _) => Valid
    case Time(_, _)               => Valid
  }

  def validateForwardReference(sections: List[Section]): ValidationResult = {
    val indexLookup: Map[FormComponentId, Int] = indexedFieldIds(sections).toMap
    val verifyValidIf = new BooleanExprValidator(indexLookup, BooleanExprWrapperType.ValidIf)(_)
    val verifyIncludeIf = new BooleanExprValidator(indexLookup, BooleanExprWrapperType.IncludeIf)(_)
    Monoid[ValidationResult]
      .combineAll(
        SectionHelper
          .pages(sections)
          .zipWithIndex
          .flatMap {
            case (page: Page, idx) =>
              val allValidfs: List[BooleanExpr] = page.allFormComponents.flatMap(_.allValidIfs).map(_.booleanExpr)
              val verifiedValidIf = allValidfs.flatMap(verifyValidIf(idx).apply)

              val verifiedIncludeIf = page.includeIf
                .map(_.booleanExpr)
                .map(verifyIncludeIf(idx).apply)
                .getOrElse(List(Valid))

              verifiedValidIf ++ verifiedIncludeIf
          })
  }

  def validate(expr: Expr, sections: List[Section]): ValidationResult = {
    val fieldNamesIds: List[FormComponentId] = fieldIds(sections)

    def checkFields(field1: Expr, field2: Expr): ValidationResult = {
      val checkField1 = validate(field1, sections)
      val checkField2 = validate(field2, sections)
      Monoid[ValidationResult].combineAll(List(checkField1, checkField2))
    }

    expr match {
      case Add(field1, field2)          => checkFields(field1, field2)
      case Subtraction(field1, field2)  => checkFields(field1, field2)
      case Multiply(field1, field2)     => checkFields(field1, field2)
      case Else(field1, field2)         => checkFields(field1, field2)
      case Sum(value)                   => validate(value, sections)
      case Count(value)                 => validate(FormCtx(value), sections)
      case HmrcRosmRegistrationCheck(_) => Valid
      case FormCtx(value) =>
        fieldNamesIds
          .contains(value)
          .validationResult(s"Form field '$value' is not defined in form template.")
      case ParamCtx(_)        => Valid
      case AuthCtx(_)         => Valid
      case UserCtx(_)         => Valid
      case Constant(_)        => Valid
      case Value              => Valid
      case FormTemplateCtx(_) => Valid
      case LinkCtx(_)         => Valid
      case DateCtx(value) =>
        val invalidFCIds = value.leafExprs
          .collect {
            case FormCtx(formComponentId) => formComponentId
          }
          .filter(!fieldNamesIds.contains(_))
          .map(_.value)
        invalidFCIds.isEmpty.validationResult(
          s"Form field(s) '${invalidFCIds.mkString(",")}' not defined in form template.")
    }
  }

  def validateEmailParameter(formTemplate: FormTemplate): ValidationResult =
    formTemplate.emailParameters.fold[ValidationResult](Valid) { emailParams =>
      val ids = fieldIds(formTemplate.sections)
      emailParams
        .collect {
          case EmailParameter(_, FormCtx(value)) if !ids.contains(value) => value
        } match {
        case Nil => Valid
        case invalidFields =>
          Invalid(
            s"The following email parameters are not fields in the form template's sections: ${invalidFields.mkString(", ")}")
      }
    }

  def validateDates(formTemplate: FormTemplate): ValidationResult =
    getAllDates(formTemplate)
      .map {
        case ConcreteDate(Year.Exact(year), Month.Exact(month), Day.Exact(day)) =>
          validateYearMonthAndDay(year, month, day)
        case ConcreteDate(Year.Any, Month.Exact(month), Day.Exact(day)) =>
          val leapYear = 2020 //makes 29th of feb valid when we dont know the year
          validateYearMonthAndDay(leapYear, month, day)
        case _ => ""
      }
      .filterNot(_ == "") match {
      case messages if messages.isEmpty  => Valid
      case messages if messages.nonEmpty => Invalid(messages.mkString(". "))
    }

  private def validateYearMonthAndDay(year: Int, month: Int, day: Int): String =
    Try(LocalDate.of(year, month, day)) match {
      case Failure(message) => message.toString
      case Success(_)       => ""
    }

  private val isRevealingChoice: FormComponent => Boolean = fc =>
    fc.`type` match {
      case _: RevealingChoice => true
      case _                  => false
  }

  private val isGroup: FormComponent => Boolean = fc =>
    fc.`type` match {
      case group: Group => true
      case _            => false
  }

  def validateGroup(formTemplate: FormTemplate): ValidationResult =
    validateComponents("Group", formTemplate)(f => {
      case Group(fields, _, _, _, _) => fields.forall(f)
    })

  def validateRevealingChoice(formTemplate: FormTemplate): ValidationResult =
    validateComponents("Revealing choice", formTemplate)(f => {
      case RevealingChoice(options, _) => options.forall(_.revealingFields.forall(f))
    })

  private def validateComponents(str: String, formTemplate: FormTemplate)(
    pf: (FormComponent => Boolean) => PartialFunction[ComponentType, Boolean]): ValidationResult = {

    val formComponents: List[FormComponent] = SectionHelper.pages(formTemplate.sections).flatMap(_.fields)

    val rcElements: (FormComponent => Boolean) => Boolean = f =>
      formComponents.map(_.`type`).collect(pf(f)).forall(identity)

    val noRevealingChoice = rcElements(fc => !isRevealingChoice(fc))
    val noGroup = rcElements(fc => !isGroup(fc))

    (noRevealingChoice, noGroup) match {
      case (true, true) => Valid
      case (false, _)   => Invalid(str + " cannot contains revealing choice as its element")
      case (_, false)   => Invalid(str + " cannot contains group as its element")
    }
  }

  def validateEmailVerification(formTemplate: FormTemplate): ValidationResult = {

    val sections = formTemplate.sections

    val indexLookup: Map[FormComponentId, Int] = indexedFieldIds(sections).toMap

    val allFormComponents: List[FormComponent] = indexedFields(sections).map { case (a, b) => a }

    val emailsWithCodes: List[(FormComponentId, FormComponentId)] =
      allFormComponents.collect { case IsEmailVerifiedBy(fcId, emailId) => (fcId, emailId) }

    val result = emailsWithCodes.map {
      case (emailField, codeField) =>
        val emailIdx = indexLookup.get(emailField) // This is guaranteed to always succeed
        val codeIdx = indexLookup.get(codeField)

        val res = for {
          e <- emailIdx
          c <- codeIdx
        } yield {
          (e <= c).validationResult(
            s"id '$codeField' named in email verification is forward reference, which is not permitted")
        }

        res.getOrElse(Invalid(s"id '$codeField' named in email verification does not exist in the form"))
    }
    Monoid[ValidationResult].combineAll(result)
  }
}

object IsEmailVerifiedBy {
  def unapply(formComponent: FormComponent): Option[(FormComponentId, FormComponentId)] = formComponent.`type` match {
    case Text(EmailVerifiedBy(fcId, _), _, _, _, _, _) => Some((formComponent.id, fcId))
    case _                                             => None
  }
}
