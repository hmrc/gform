/*
 * Copyright 2017 HM Revenue & Customs
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
import play.api.libs.json.{ JsError, JsSuccess }
import uk.gov.hmrc.gform.core.{ Invalid, Opt, Valid, ValidationResult }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.form.FormField

import scala.collection.immutable.List

object FormTemplateValidator {

  def validateUniqueFields(sectionsList: List[Section]): ValidationResult = {
    val fieldIds: List[FieldId] = sectionsList.flatMap(_.fields.map(_.id))
    val duplicates: List[FieldId] = fieldIds.groupBy(identity).collect { case (fId, List(_, _, _*)) => fId }.toList

    duplicates.isEmpty match {
      case true => Valid
      case false => Invalid(s"Some FieldIds are defined more than once: ${duplicates.map(_.value)}")
    }
  }

  def validateChoiceHelpText(sectionsList: List[Section]): ValidationResult = {
    val choiceFieldIdMap: Map[FieldId, Boolean] = sectionsList.flatMap(_.fields).map(fv => (fv.id, fv.`type`))
      .collect {
        case (fId, Choice(_, options, _, _, helpTextList @ Some(x :: xs))) =>
          (fId, options.toList.size.equals(helpTextList.getOrElse(List.empty).size))
      }
      .toMap

    val choiceFieldIdResult = choiceFieldIdMap.filter(value => value._2.equals(false))

    choiceFieldIdResult.isEmpty match {
      case true => Valid
      case false => Invalid(s"Choice components doesn't have equal number of choices and help texts ${choiceFieldIdResult.keys.toList}")
    }
  }

  def validateRepeatingSectionFields(sectionList: List[Section]): ValidationResult = {
    val results = sectionList.map { section =>
      (section.repeatsMax, section.repeatsMin) match {
        case (Some(repMax), Some(repMin)) if !repMax.equals(repMin) =>
          Invalid(s"The repeatsMax and repeatsMin fields must be the same in a repeating section: repeatsMax=[$repMax], repeatsMin=[$repMin]")
        case (Some(_), None) | (None, Some(_)) =>
          val repMax = section.repeatsMax.getOrElse("")
          val repMin = section.repeatsMin.getOrElse("")
          Invalid(s"Both repeatsMax and repeatsMin fields must be provided in a repeating section: repeatsMax=[$repMax], repeatsMin=[$repMin]")
        case _ => Valid
      }
    }
    Monoid[ValidationResult].combineAll(results)
  }

  private def getMandatoryAndOptionalFields(section: Section): (Set[FieldId], Set[FieldId]) = {
    SectionHelper.atomicFields(section, Map.empty).foldLeft((Set.empty[FieldId], Set.empty[FieldId])) {
      case ((mandatoryAcc, optionalAcc), field) =>
        (field.`type`, field.mandatory) match {
          case (Address(_), _) => (mandatoryAcc ++ Address.mandatoryFields(field.id), optionalAcc ++ Address.optionalFields(field.id))
          case (Date(_, _, _), _) => (mandatoryAcc ++ Date.fields(field.id), optionalAcc)
          case (Text(_, _, _), true) => (mandatoryAcc + field.id, optionalAcc)
          case (Text(_, _, _), false) => (mandatoryAcc, optionalAcc + field.id)
          case (_, true) => (mandatoryAcc + field.id, optionalAcc)
          case (_, false) => (mandatoryAcc, optionalAcc + field.id)
        }
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
    val formFieldIds: Set[FieldId] = formFields.map(_.id).toSet
    val sectionOpt: Option[Section] = sections.find { section =>

      val (mandatorySectionIds, optionalSectionIds) = getMandatoryAndOptionalFields(section)

      val missingMandatoryFields = mandatorySectionIds diff formFieldIds

      val optionalFieldsFromSubmission = RepeatingComponentService.discardRepeatingFields(
        formFieldIds diff mandatorySectionIds,
        mandatorySectionIds,
        optionalSectionIds
      )

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

  def validate(exprs: List[ComponentType], formTemplate: FormTemplate): ValidationResult = {
    val results = exprs.map(validate(_, formTemplate))
    Monoid[ValidationResult].combineAll(results)
  }

  def validate(componentType: ComponentType, formTemplate: FormTemplate): ValidationResult = componentType match {
    case UkSortCode(expr) => validate(expr, formTemplate)
    case Text(_, expr, _) => validate(expr, formTemplate)
    case Date(_, _, _) => Valid
    case Address(_) => Valid
    case Choice(_, _, _, _, _) => Valid
    case Group(fvs, _, _, _, _, _) => FormTemplateValidator.validate(fvs.map(_.`type`), formTemplate)
    case FileUpload() => Valid
    case InformationMessage(_, _) => Valid
  }

  def validate(booleanExpr: BooleanExpr, formTemplate: FormTemplate): ValidationResult = {
    val fieldNamesIds: List[FieldId] = formTemplate.sections.flatMap(_.fields.map(_.id))

    def checkFields(field1: Expr, field2: Expr): ValidationResult = {
      val checkField1 = validate(field1, formTemplate)
      val checkField2 = validate(field2, formTemplate)
      Monoid[ValidationResult].combineAll(List(checkField1, checkField2))
    }

    booleanExpr match {
      case Equals(field1, field2) => checkFields(field1, field2)
      case _ => Valid
    }
  }

  def validate(expr: Expr, formTemplate: FormTemplate): ValidationResult = {
    val fieldNamesIds: List[FieldId] = formTemplate.sections.flatMap(_.fields.map(_.id))

    def checkFields(field1: Expr, field2: Expr): ValidationResult = {
      val checkField1 = validate(field1, formTemplate)
      val checkField2 = validate(field2, formTemplate)
      Monoid[ValidationResult].combineAll(List(checkField1, checkField2))
    }

    expr match {
      case Add(field1, field2) => checkFields(field1, field2)
      case Multiply(field1, field2) => checkFields(field1, field2)
      case FormCtx(value) =>
        if (fieldNamesIds.map(_.value).contains(value))
          Valid
        else
          Invalid(s"Form field '$value' is not defined in form template.")
      case AuthCtx(value) => Valid
      case EeittCtx(value) => Valid
      case Constant(_) => Valid
    }
  }

}
