/*
 * Copyright 2019 HM Revenue & Customs
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

import cats.Monoid
import cats.data.NonEmptyList
import cats.implicits._
import javax.xml.bind.util.ValidationEventCollector
import scalax.collection.Graph
import scalax.collection.GraphEdge._
import uk.gov.hmrc.gform.core.{ Invalid, Opt, Valid, ValidationResult }
import uk.gov.hmrc.gform.core.ValidationResult.{ BooleanToValidationResultSyntax, validationResultMonoid }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId, Destinations }
import uk.gov.hmrc.gform.sharedmodel.graph.DependencyGraph._

import scala.Function.const
import scala.collection.immutable.List

object FormTemplateValidator {

  def someFieldsAreDefinedMoreThanOnce(duplicates: Set[FormComponentId]) =
    s"Some FieldIds are defined more than once: ${duplicates.toList.sortBy(_.value).map(_.value)}"

  def validateUniqueFields(sectionsList: List[Section]): ValidationResult = {
    val fieldIds = sectionsList.flatMap(_.fields.map(_.id))
    val duplicates = fieldIds.groupBy(identity).collect { case (fId, List(_, _, _*)) => fId }.toSet
    duplicates.isEmpty.validationResult(someFieldsAreDefinedMoreThanOnce(duplicates))
  }

  def someDestinationIdsAreUsedMoreThanOnce(duplicates: Set[DestinationId]) =
    s"Some DestinationIds are defined more than once: ${duplicates.toList.sortBy(_.id).map(_.value)}"

  def validateUniqueDestinationIds(destinations: Destinations): ValidationResult = destinations match {
    case _: Destinations.DmsSubmission => Valid

    case destinationList: Destinations.DestinationList =>
      val destinationIds = destinationList.destinations.map(_.id)
      val duplicates = destinationIds.toList.groupBy(identity).collect { case (dId, List(_, _, _*)) => dId }.toSet
      duplicates.isEmpty.validationResult(someDestinationIdsAreUsedMoreThanOnce(duplicates))
  }

  val oneOrMoreHmrcDestinationsRequired: String = "There must be at least one hmrcDms destination"

  def validateOneOrMoreHmrcDmsDestination(destinations: Destinations): ValidationResult = destinations match {
    case _: Destinations.DmsSubmission => Valid

    case destinationList: Destinations.DestinationList =>
      val hmrcDmsDestinations = destinationList.destinations.collect { case d: Destination.HmrcDms => d }
      (!hmrcDmsDestinations.isEmpty)
        .validationResult(oneOrMoreHmrcDestinationsRequired)
  }

  def validateChoiceHelpText(sectionsList: List[Section]): ValidationResult = {
    val choiceFieldIdMap: Map[FormComponentId, Boolean] = sectionsList
      .flatMap(_.fields)
      .map(fv => (fv.id, fv.`type`))
      .collect {
        case (fId, Choice(_, options, _, _, helpTextList @ Some(x :: xs))) =>
          (fId, options.toList.size.equals(helpTextList.getOrElse(List.empty).size))
      }
      .toMap

    val choiceFieldIdResult = choiceFieldIdMap.filter(value => value._2.equals(false))

    choiceFieldIdResult.isEmpty.validationResult(
      s"Choice components doesn't have equal number of choices and help texts ${choiceFieldIdResult.keys.toList}")
  }

  def validateRepeatingSectionFields(sectionList: List[Section]): ValidationResult = {
    val results = sectionList.map { section =>
      (section.repeatsMax, section.repeatsMin) match {
        case (Some(repMax), Some(repMin)) if !repMax.equals(repMin) =>
          Invalid(
            s"The repeatsMax and repeatsMin fields must be the same in a repeating section: repeatsMax=[$repMax], repeatsMin=[$repMin]")
        case (Some(_), None) | (None, Some(_)) =>
          val repMax = section.repeatsMax.getOrElse("")
          val repMin = section.repeatsMin.getOrElse("")
          Invalid(
            s"Both repeatsMax and repeatsMin fields must be provided in a repeating section: repeatsMax=[$repMax], repeatsMin=[$repMin]")
        case _ => Valid
      }
    }
    Monoid[ValidationResult].combineAll(results)
  }

  private def getMandatoryAndOptionalFields(section: Section): (Set[FormComponentId], Set[FormComponentId]) =
    SectionHelper.atomicFields(section, Map.empty).foldLeft((Set.empty[FormComponentId], Set.empty[FormComponentId])) {
      case ((mandatoryAcc, optionalAcc), field) =>
        (field.`type`, field.mandatory) match {
          case (Address(_), _) =>
            (mandatoryAcc ++ Address.mandatoryFields(field.id), optionalAcc ++ Address.optionalFields(field.id))
          case (Date(_, _, _), _)     => (mandatoryAcc ++ Date.fields(field.id), optionalAcc)
          case (Text(_, _, _), true)  => (mandatoryAcc + field.id, optionalAcc)
          case (Text(_, _, _), false) => (mandatoryAcc, optionalAcc + field.id)
          case (_, true)              => (mandatoryAcc + field.id, optionalAcc)
          case (_, false)             => (mandatoryAcc, optionalAcc + field.id)
        }
    }

  /**
    * Tries to find section in form template corresponding to submitted data.
    *
    * Section is determined by these rules:
    * - all mandatory fields from the section must be present in submission
    * - optional fields from section don't need to be present in submission
    * - presence of any other field than those from form template is resulting in failed location of a section
    */
  def getMatchingSection(formFields: Seq[FormField], sections: Seq[Section]): Opt[Section] = {
    val formFieldIds: Set[FormComponentId] = formFields.map(_.id).toSet
    val sectionOpt: Option[Section] = sections.find { section =>
      val (mandatorySectionIds, optionalSectionIds) = getMandatoryAndOptionalFields(section)

      val missingMandatoryFields = mandatorySectionIds diff formFieldIds

      val optionalFieldsFromSubmission =
        RepeatingComponentService
          .discardRepeatingFields(formFieldIds diff mandatorySectionIds, mandatorySectionIds, optionalSectionIds)

      val fieldWhichAreNotFromFormTemplate = optionalFieldsFromSubmission diff optionalSectionIds

      missingMandatoryFields.isEmpty && fieldWhichAreNotFromFormTemplate.isEmpty
    }

    sectionOpt match {
      case Some(section) => Right(section)
      case None =>
        val sectionsForPrint = sections.map(_.fields.map(_.id))

        Left(UnexpectedState(s"""|Cannot find a section corresponding to the formFields
                                 |FormFields: $formFieldIds
                                 |Sections: $sectionsForPrint""".stripMargin))
    }
  }

  val userContextComponentType: List[FormComponent] => Boolean =
    enrolledIdentifierComponents =>
      enrolledIdentifierComponents.collect {
        case expr @ HasExpr(SingleExpr(UserCtx(EnrolledIdentifier))) => expr
      }.nonEmpty

  def validateEnrolmentIdentifier(formTemplate: FormTemplate, f: List[FormComponent] => Boolean): ValidationResult =
    if (f(formTemplate.expandFormTemplate.allFCs)) {
      formTemplate.authConfig match {
        case HmrcEnrolmentModule(_) | HmrcAgentWithEnrolmentModule(_, _) => Valid
        case _                                                           => Invalid("You used '${user.enrolledIdentifier}' but you didn't provide 'serviceId'.")
      }
    } else Valid

  def validateEnrolmentSection(formTemplate: FormTemplate): ValidationResult =
    formTemplate.authConfig match {
      case HasEnrolmentSection(_, enrolmentSection) =>
        val fcIds = enrolmentSection.fields.map(_.id).map(_.value).toSet
        val ctxs = enrolmentSection.identifiers.map(_.value.value).toList.toSet ++ enrolmentSection.verifiers
          .map(_.value.value)
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
    constructDepencyGraph(graph).bimap(const(Invalid(s"Graph contains cycle ${graph.findCycle}")), const(Valid)).merge
  }

  def validate(exprs: List[ComponentType], formTemplate: FormTemplate): ValidationResult = {
    val results = exprs.map(validate(_, formTemplate))
    Monoid[ValidationResult].combineAll(results)
  }

  def validate(componentType: ComponentType, formTemplate: FormTemplate): ValidationResult = componentType match {
    case HasExpr(SingleExpr(expr))     => validate(expr, formTemplate.sections)
    case HasExpr(MultipleExpr(fields)) => Valid
    case Date(_, _, _)                 => Valid
    case Address(_)                    => Valid
    case Choice(_, _, _, _, _)         => Valid
    case HmrcTaxPeriod(_, _, _)        => Valid
    case Group(fvs, _, _, _, _, _)     => validate(fvs.map(_.`type`), formTemplate)
    case FileUpload()                  => Valid
    case InformationMessage(_, _)      => Valid
  }

  def validateForwardReference(sections: List[Section]): ValidationResult = {
    val fieldNamesIds: Map[FormComponentId, Int] = sections.zipWithIndex.flatMap {
      case (element, idx) => element.fields.map(_.id -> idx)
    }.toMap

    def boolean(includeIf: BooleanExpr, idx: Int): List[ValidationResult] = includeIf match {
      case Equals(left, right) =>
        (evalExpr(left) ::: evalExpr(right))
          .map(
            id =>
              fieldNamesIds
                .get(id)
                .map(idIdx => (idIdx < idx).validationResult("Forward referencing is not permitted."))
                .getOrElse(Invalid(s"id named in include if expression does not exist in form ${id.value}")))
      case Or(left, right)  => boolean(left, idx) ::: boolean(right, idx)
      case And(left, right) => boolean(left, idx) ::: boolean(right, idx)
      case _                => List(Valid)
    }

    Monoid[ValidationResult].combineAll(
      sections.zipWithIndex
        .map(section => section._1.includeIf -> section._2)
        .collect {
          case (Some(element), idx) => element.expr -> idx
        }
        .flatMap(elements => boolean(elements._1, elements._2)))
  }

  def validate(expr: Expr, sections: List[Section]): ValidationResult = {
    val fieldNamesIds: List[FormComponentId] = sections
      .flatMap(_.fields.map(_.id)) ::: sections
      .flatMap(
        _.fields
          .map(_.`type`)
          .collect {
            case Group(fields, _, _, _, _, _) => fields.map(_.id)
          })
      .flatten

    def checkFields(field1: Expr, field2: Expr): ValidationResult = {
      val checkField1 = validate(field1, sections)
      val checkField2 = validate(field2, sections)
      Monoid[ValidationResult].combineAll(List(checkField1, checkField2))
    }

    expr match {
      case Add(field1, field2)         => checkFields(field1, field2)
      case Subtraction(field1, field2) => checkFields(field1, field2)
      case Multiply(field1, field2)    => checkFields(field1, field2)
      case Sum(value)                  => validate(value, sections)
      case FormCtx(value) =>
        fieldNamesIds
          .map(_.value)
          .contains(value)
          .validationResult(s"Form field '$value' is not defined in form template.")
      case AuthCtx(_)          => Valid
      case EeittCtx(_)         => Valid
      case UserCtx(_)          => Valid
      case Constant(_)         => Valid
      case Value               => Valid
      case SubmissionReference => Valid
    }
  }

  def getAllFieldIdsFromFormTemplate(formTemplate: FormTemplate): List[FormComponentId] = {

    val sectionsFields = extractFieldIds(formTemplate.sections.flatMap(_.fields))
    val declarationSectionFields = extractFieldIds(formTemplate.declarationSection.fields)

    sectionsFields ::: declarationSectionFields

  }

  private def extractFieldIds(fields: List[FormComponent]): List[FormComponentId] = fields.flatMap(extractFieldIds)

  private def extractFieldIds(field: FormComponent): List[FormComponentId] = field.`type` match {
    case group: Group => group.fields.map(_.id)
    case _            => List(field.id)
  }

  def validateEmailParameter(formTemplate: FormTemplate): ValidationResult =
    formTemplate.emailParameters.fold[ValidationResult](Valid) { emailParams =>
      val ids = getAllFieldIdsFromFormTemplate(formTemplate)
      emailParams
        .filterNot(parameter => ids.contains(parameter.value.toFieldId)) match {
        case Nil => Valid
        case invalidFields =>
          Invalid(
            s"The following email parameters are not fields in the form template's sections or the declaration section: ${invalidFields
              .map(_.value.toFieldId)}")
      }
    }

  private def evalExpr(expr: Expr): List[FormComponentId] = expr match {
    case Add(left, right)         => evalExpr(left) ::: evalExpr(right)
    case Subtraction(left, right) => evalExpr(left) ::: evalExpr(right)
    case Multiply(left, right)    => evalExpr(left) ::: evalExpr(right) //TODO find a way to stop code repeating.
    case Sum(field1)              => evalExpr(field1)
    case id: FormCtx              => List(id.toFieldId)
    case _                        => Nil
  }
}
